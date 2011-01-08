-module(telehash).
-include("../include/telehash.hrl").
-behaviour(gen_server).

% API
-export([start_link/1, stop/1, from_json/1, get_state/1]).

% gen_server
-export([init/1, handle_info/2, handle_call/3, terminate/2,
    code_change/3, handle_cast/2]).



%%% %%%
%%% Start up functions
%%% %%%

start_link(Opts) -> gen_server:start_link(?MODULE, Opts, []).

init([Key, Seed = {_SeedHost, _SeedPort}]) -> init([Key, Seed, 0]);
init([Key]) -> init([Key, 0]);

%% init/1 with seed information will start the bootstrap process
init([Key, {SeedHost, SeedPort}, Port]) ->
    case gen_udp:open(Port, [binary]) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket, self()),
            gen_udp:send(Socket, SeedHost, SeedPort, bootstrap_packet(Key)),
            % TODO: Interpret response; ok | {error, Reason}
            {ok, #switch{socket=Socket, hash=hash(Key)}}
    end;

%% init/1 with only a Key, implies that we are a seed
init([Key, Port]) ->
    case gen_udp:open(Port, [binary]) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket, self()),
            {ok, #switch{socket=Socket, hash=hash(Key)}};
        {error, Reason} -> {stop, Reason}
    end.

bootstrap_packet(Key) ->
    to_json({struct,
        [{'+end', hash(Key)}]
    }).



%%% %%%
%%% Interface functions
%%% %%%

% add_tap(Pid, Tap) ->
%     gen_server:call(Pid, {add_self_tap, Tap}).

stop(Pid) -> gen_server:call(Pid, terminate).

get_state(Pid) -> gen_server:call(Pid, get_state).



%%% %%%
%%% TeleHash/UDP callbacks
%%%

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    StrIP = ip_to_list(IP),
    io:format("~s:~b sent: ~s", [StrIP, Port, Packet]),
    JsObj = (catch from_json(Packet)),
    io:format("    Decoded json: ~p~n", [JsObj]),
    NewPacket = to_json({struct, [{foo, bar}, {rab, oof}]}),
    io:format("    Sending ~s to ~s:~b~n", [NewPacket, StrIP, Port]),
    case gen_udp:send(Socket, IP, Port, NewPacket) of
        ok -> ok;
        {error, Reason} -> io:format("    Send failed: ~p~n", [Reason])
    end,
    {noreply, handle_telex(mk_telex(JsObj), {IP, Port, State})}.



%%% %%%
%%% The main Telex handler
%%% %%%

%% handle_telex/2
%% In general, takes a #telex record, tuple of sender and state info,
%% and returns a #switch record

%% When we don't have a valid telex, drop it.
handle_telex(undefined, {_,_,S}) -> S;

%% When we don't yet know our public IP:PORT, complete bootstrap if possible
handle_telex(T = #telex{dict=Dict}, {IP, Port,S=#switch{ipp=undefined}}) ->
    case orddict:find(<<"_to">>, Dict) of
        {ok, IPPStr} ->
            [BinSelfIP, SelfPort] = binary:split(IPPStr, <<":">>),
            % We now know our public IP:PORT, add it to our state, and continue
            handle_telex(T, {IP, Port,
                S#switch{ipp={ip_from_binary(BinSelfIP),
                    list_to_integer(binary_to_list(SelfPort))}}});
        error -> io:format("Still no public IP:PORT..."), S
    end;

handle_telex(#telex{}, {_, _, S= #switch{}}) -> S.



%%% %%%
%%% Utility and debugging functions
%%% %%%

handle_call(get_state, _From, State) ->
    {reply, State, State};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

terminate(_, State) ->
    catch gen_udp:close(State#switch.socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_cast(Msg, State) ->
    io:format("Received unknown cast ~p", [Msg]),
    {noreply, State}.



%%% %%%
%%% Data utility functions (for json and hashing)
%%% %%%

%% from_json/1 and to_json/1
%% mochijson2 objects look like this:
%% > O = {struct,
%%          [
%%              {'+end', <<"abcd">>},
%%              {".see", <<"1.23.58.63:8302">>}
%%          ]
%%       }.
%% > BS = to_json(Obj).
%% <<"{\"+end\":\"abcd\",\".see\":\"1.23.58.63:8302\"}">>
%% > io:format(BS).
%% {"+end":"abcd",".see":"1.23.58.63:8302"}
%% ok
from_json(BinStr) -> mochijson2:decode(BinStr).
to_json(Term) -> iolist_to_binary(mochijson2:encode(Term)).

%% to_hex/1
%% Takes a binary digest like those returned by crypto:sha/1
%% and returns a binary string of the digest converted to hexadecimal
to_hex(Digest) ->
    List = [io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Digest)],
    Str = lists:flatten(List),
    list_to_binary(Str).

%% hash/1
%% Takes binary or iolist Data, produces a binary string of the SHA1 hex digest
hash(Data) ->
    to_hex(crypto:sha(Data)).

%% ip_to_list/1
%% Simply converts a tuple ipv4 address to a string/list
ip_to_list({A, B, C, D}) -> io_lib:format("~b.~b.~b.~b", [A, B, C, D]).

%% ip_from_list/1
%% Simply converts a string ipv4 address to tuple form
ip_from_binary(IpStr) ->
    Convert = fun(BinStr) -> list_to_integer(binary_to_list(BinStr)) end,
    [A, B, C, D] = lists:map(Convert,
        binary:split(IpStr, <<".">>, [global])),
    {A, B, C, D}.

%% mk_telex/1
%% Takes a JSON object returned by from_json
%% Returns a more functional #telex record
mk_telex({struct, AList}) -> #telex{dict=orddict:from_list(AList)};
%% If that doesn't work, return undefined. This usually happens if
%% the JSON is invalid or it isn't an object (i.e. is an array or other element)
mk_telex(_) -> undefined.