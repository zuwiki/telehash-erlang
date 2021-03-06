-module(telehash_test).

-include("../include/telehash.hrl").

-compile(export_all).

start() ->
    process_flag(trap_exit, true),
    spawn_link(?MODULE, run_tests, []),
    receive
        X -> io:format("Stopped test with:~n~p~n", [X])
    end,
    halt().

run_tests() ->
    O = lists:map(fun(Test) ->
            {Test, catch apply(?MODULE, Test, [])}
        end,
        tests()),
    io:format("~p~n", [O]).

tests() ->
    [
    % Data utilities
    test_hash_ipp,
    test_hash_string,
    test_hash_switch,
    test_to_json_term,
    test_to_json_telex,
    % Record utilities
    test_switch_store_endpoint,
    test_hash_distance
    ].


%%% ----
%%% Let's kick things off with some simple data utility tests
%%% ----

%% telehash:hash/1 tests
test_hash_string() ->
    ExpectedBinDigest = crypto:sha("this is a test"),
    ExpectedBinDigest = telehash:hash("this is a test"),
    "telehash:hash(String) is identical to crypto:sha(String)".
test_hash_ipp() ->
    ExpectedBinDigest = crypto:sha("74.125.127.99:42424"),
    ExpectedBinDigest = telehash:hash({{74,125,127,99}, 42424}),
    "telehash:hash(IPP) hashes the string form IP:PORT".
test_hash_switch() ->
    ExpectedBinDigest = crypto:sha("74.125.127.99:42424"),
    ExpectedBinDigest = telehash:hash(#switch{ipp={{74,125,127,99}, 42424}}),
    "telehash:hash(S=#switch{}) hashes the switch's IPP".

%% telehash:to_json/1 tests
test_to_json_term() ->
    ExpectedJson = <<"{\"foo\":\"bar\",\"baz\":3,\"zag\":[3,2,4]}">>,
    ExpectedJson = telehash:to_json({struct,
            [{"foo", <<"bar">>},
             {"baz", 3},
             {"zag", [3,2,4]}]}),
    "telehash:to_json(Term) makes JSON bitstr given {struct,[{k,v}]} term".
test_to_json_telex() ->
    throw('NOT_IMPLEMENTED').


%%% ----
%%% Time to get a bit more serious: now it's record data
%%% ----

%% #switch{} manipulation tests
test_switch_store_endpoint() ->
    End1 = #endpoint{ipp={{1,2,3,4},5555}},
    End2 = #endpoint{ipp={{5,4,3,2},1111}},
    % TODO: Ideally this wouldn't know about storage implementation
    ExpectedEnds = orddict:from_list([
        {crypto:sha("1.2.3.4:5555"), End1},
        {crypto:sha("5.4.3.2:1111"), End2}
    ]),
    Switch1 = telehash:store_end(End1, #switch{}),
    Switch2 = telehash:store_end(End2, Switch1),
    ExpectedEnds = Switch2#switch.ends,
    "telehash:store_end(E=#endpoint{}, S=#switch{}) stores end by hash".


%%% ----
%%% Okay, now some math-y type things that are more functionally important
%%% ----

%% DHT metric tests
test_hash_distance() ->
    Hash1 = telehash:hash({{1,2,3,4},5555}),
    Hash2 = telehash:hash({{5,4,3,2},1111}),
    throw('NOT_IMPLEMENTED').
