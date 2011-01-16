-module(telehash).
-author('zuwiki@zuwiki.net').

-include("../include/telehash.hrl").
-behaviour(gen_server).

% server API
-export([start_link/0, start_link/1, stop/1, get_state/1]).

% gen_server
-export([init/1, handle_info/2, handle_call/3, terminate/2,
    code_change/3, handle_cast/2]).

% data utility API
-export([from_json/1, to_json/1, hash/1, hash_ipp/1, mk_telex/1,
    binary_to_ipp/1, ipp_to_binary/1]).

% testing API
-export([handle_telex/3]).


%%% %%%
%%% Start up functions
%%% %%%

start_link() -> start_link([]).

start_link(Opts) -> gen_server:start_link(?MODULE, Opts, []).

init([Seed = {_SeedHost, _SeedPort}]) -> init([Seed, 0]);
init([]) -> init([0]);

%% init/1 with seed information will start the bootstrap process
init([{SeedHost, SeedPort}, Port]) ->
    case gen_udp:open(Port, [binary]) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket, self()),
            gen_udp:send(Socket, SeedHost, SeedPort, bootstrap_packet()),
            % TODO: Interpret response; ok | {error, Reason}
            {ok, #switch{socket=Socket}}
    end;

%% init/1 without seed information implies we are a seed and won't bootstrap
init([Port]) ->
    case gen_udp:open(Port, [binary]) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket, self()),
            {ok, #switch{socket=Socket}};
        {error, Reason} -> {stop, Reason}
    end.

bootstrap_packet() ->
    to_json({struct,
        [{'+end', hash("bootstrap")}]
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
    JsObj = (catch from_json(Packet)),
%    StrIP = ip_to_list(IP),
%    io:format("~s:~b sent: ~s", [StrIP, Port, Packet]),
%    io:format("    Decoded json: ~p~n", [JsObj]),
%    NewPacket = to_json({struct, [{foo, bar}, {rab, oof}]}),
%    io:format("    Sending ~s to ~s:~b~n", [NewPacket, StrIP, Port]),
%    case gen_udp:send(Socket, IP, Port, NewPacket) of
%        ok -> ok;
%        {error, Reason} -> io:format("    Send failed: ~p~n", [Reason])
%    end,
    NewState = case handle_telex(mk_telex(JsObj), {IP, Port}, State) of
        {reply, JsReply, S} ->
            gen_udp:send(Socket, IP, Port, JsReply),
            S;
        {noreply, S} -> S
    end,
    {noreply, NewState}.



%%% %%%
%%% The main Telex handler
%%% %%%

%% handle_telex/3
%% In general, takes a #telex record, an IPP, and state info,
%% and returns a #switch record

%% When we don't have a valid telex, drop it.
handle_telex(undefined, _IPP, S) -> {noreply, S};

%% When we don't yet know our public IP:PORT, complete bootstrap if possible
%% TODO: disregard if there is a _hop; don't want the wrong IP:PORT!
handle_telex(T=#telex{dict=Dict},
    IPP, S=#switch{ipp=undefined}) ->
    case orddict:find(<<"_to">>, Dict) of
        {ok, IPPStr} ->
            error_logger:info_msg("Learned external IP:P=~s~n", [IPPStr]),
            handle_telex(T, IPP, S#switch{ipp=binary_to_ipp(IPPStr)});
        error ->
            error_logger:info_msg("Received a Telex but still have no IP:P~n"),
            {noreply, S}
    end;

handle_telex(#telex{}, _IPP, S=#switch{}) -> {noreply, S}.


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
    error_logger:info_msg("Received unknown cast ~p", [Msg]),
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

%% hash_ipp/1
%% Takes a tuple-form IPP, produces the IP:P hash.
hash_ipp(IPP) -> hash(ipp_to_binary(IPP)).

%% ip_to_list/1
%% Simply converts a tuple ipv4 address to a string/list
ip_to_list({A, B, C, D}) -> io_lib:format("~b.~b.~b.~b", [A, B, C, D]).

%% ip_from_list/1
%% Simply converts a string ipv4 address to tuple form
binary_to_ipp(IPPStr) ->
    [IpStr, Port] = binary:split(IPPStr, <<":">>),
    Convert = fun(BinStr) -> list_to_integer(binary_to_list(BinStr)) end,
    [A, B, C, D] = lists:map(Convert,
        binary:split(IpStr, <<".">>, [global])),
    {{A, B, C, D}, list_to_integer(binary_to_list(Port))}.

ipp_to_binary({IP, Port}) ->
    list_to_binary(lists:concat([ip_to_list(IP), ":", integer_to_list(Port)])).

%% mk_telex/1
%% Takes a JSON object returned by from_json
%% Returns a more functional #telex record
mk_telex({struct, AList}) -> #telex{dict=orddict:from_list(AList)};
%% If that doesn't work, return undefined. This usually happens if
%% the JSON is invalid or it isn't an object (i.e. is an array or other element)
mk_telex(_) -> undefined.