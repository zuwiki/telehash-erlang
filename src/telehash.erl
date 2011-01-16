-module(telehash).
-author('zuwiki@zuwiki.net').

-include("../include/telehash.hrl").
-behaviour(gen_server).

% server API
-export([start_link/0, start_link/1, stop/1]).

% gen_server
-export([init/1, handle_info/2, handle_call/3, terminate/2,
    code_change/3, handle_cast/2]).

% data utility API
-export([from_json/1, to_json/1, hash/1, mk_telex/1,
    binary_to_ipp/1, ipp_to_binary/1, telex/1, set/3, has/2]).

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
        [{"+end", hash("bootstrap")}]
    }).



%%% %%%
%%% Interface functions
%%% %%%

stop(Pid) -> gen_server:call(Pid, terminate).

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
%%% TeleHash/UDP callbacks
%%% %%%

handle_info({udp, Socket, IP, Port, Packet}, State) ->
    JsObj = (catch from_json(Packet)),
    IPP = {IP, Port},
    error_logger:info_msg("<< ~s", [ipp_to_binary(IPP)]),
    NewState = case handle_telex(mk_telex(JsObj), {IP, Port}, State) of
        {reply, JsReply, S} ->
            gen_udp:send(Socket, IP, Port, JsReply),
            error_logger:info_msg(">> ~s", [ipp_to_binary(IPP)]),
            S;
        {noreply, S} -> S
    end,
    {noreply, NewState}.



%%% %%%
%%% The main Telex handler
%%% %%%

%% handle_telex/3
%% In general, takes a #telex record, an IPP, and a #switch record,
%% and returns a tuple of one of two forms:
%%      {noreply, S}
%%      {reply, JSON, S}
%% JSON is a bit string to be sent to the IPP, S is a #switch for the new state

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

handle_telex(T=#telex{}, IPP, S=#switch{}) ->
    % TODO: Add checking the number of hops
    case {has("+end", T), line_active(IPP, S)} of
        % The remote End is trying to join the network; .see some Ends
        {true, false} ->
            % Find Ends close to the remote End
            Sees = nearby(hash(IPP), hash(S), S),
            Out = set(".see", lists:sublist(Sees, 5), telex(IPP)),
            {reply, to_json(Out), S};
        _ -> {noreply, S}
    end.



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
%%
%% It is worth mentioning that passing a #telex record will translate it to
%% {struct, AList} form and then translate to JSON.
from_json(BinStr) -> mochijson2:decode(BinStr).
to_json(#telex{dict=Dict}) -> to_json({struct, orddict:to_list(Dict)});
to_json(Term) -> iolist_to_binary(mochijson2:encode(Term)).

%% to_hex/1
%% Takes a binary digest like those returned by crypto:sha/1
%% and returns a binary string of the digest converted to hexadecimal
to_hex(Digest) ->
    List = [io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(Digest)],
    Str = lists:flatten(List),
    list_to_binary(Str).

%% hash/1
%% Takes various datatypes and produces a binary string of the SHA1 hex digest
hash(#switch{hash=Hash}) when Hash /= undefined -> Hash;
hash(#switch{ipp=IPP}) when IPP /= undefined -> hash(IPP);
hash(IPP={{_,_,_,_},_}) -> hash(ipp_to_binary(IPP));
hash(Data) when is_binary(Data) or is_list(Data) ->
    to_hex(crypto:sha(Data)).

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


%%% %%%
%%% Telex data operations
%%% %%%

%% mk_telex/1
%% Takes a JSON object returned by from_json
%% Returns a more functional #telex record
mk_telex({struct, AList}) -> #telex{dict=orddict:from_list(AList)};
%% If that doesn't work, return undefined. This usually happens if
%% the JSON is invalid or it isn't an object (i.e. is an array or other element)
mk_telex(_) -> undefined.

%% telex/1
%% Makes a brand new Telex setting the _to field to the given IPP
%% TODO: Should also lookup the other End and at a _br field
telex(ToIPP) ->
    set("_to", ipp_to_binary(ToIPP), #telex{}).

%% set/3
%% Takes a key, value, and telex record, adding the key:val to the telex
%% Gaurantees storage with keys as bit strings
set(Key, Val, Tel) when is_list(Key) -> set(list_to_binary(Key), Val, Tel);
set(Key, Val, Tel=#telex{dict=Dict}) ->
    Tel#telex{dict=orddict:store(Key, Val, Dict)}.

%% has/2
%% Takes a key and #telex and returns true/false depending on whether the given
%% #telex includes the given key.
has(Key, Tel) when is_list(Key) -> has(list_to_binary(Key), Tel);
has(Key, #telex{dict=Dict}) ->
    orddict:is_key(Key, Dict).

%% line_active/2
%% Takes an IPP and a #switch, then determines if there is a recorded #end and
%% returns true if it both exists and has a defined line
line_active(IPP, #switch{ends=Ends}) ->
    case orddict:find(hash(IPP), Ends) of
        {ok, End} -> End#rend.line /= undefined;
        error -> false
    end.

%% nearby/3
%% Takes an End, a Vis starting point, and a #switch. Returns all the #rends in
%% the #switch whose distance from the End is less than the distance between the %% End and the Vis. TODO: clarify the documentation here.
nearby(End, Vis, S=#switch{ends=Ends}) ->
    Dist = crypto:exor(End, Vis),
    Sees = orddict:filter(
        fun(Hash, _PossibleEnd) ->
            crypto:exor(End, Hash) < Dist
        end, Ends),
    lists:map(fun(H) -> get_ipp(H, S) end, orddict:fetch_keys(Sees)).

%% get_ipp/2
%% Takes a Hash for an end and a #switch. Finds the corresponding End in the
%% #switch and returns its IPP if it exists, undefined if it doesn't.
get_ipp(Hash, #switch{ends=Ends}) ->
    case orddict:find(Hash, Ends) of
        {ok, End} -> End#rend.ipp;
        error -> undefined
    end.