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
    throw(O).

tests() ->
    [
    test_hash_ipp,
    test_hash_string,
    test_hash_switch,
    switch_hash_storage
    ].


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

switch_hash_storage() ->
    throw("not implemented").