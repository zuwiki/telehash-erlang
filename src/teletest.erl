-module(teletest).

-include("../include/telehash.hrl").

-export([start/1]).

start(N) ->
    {ok, P} = telehash:start_link([9000+N]),
    start(N-1, 9000+N, [P]).

start(0, _SeedPort, Pids) -> Pids;
start(N, SeedPort, Pids) ->
    {ok, P} = telehash:start_link([{{127,0,0,1}, SeedPort}, 9000+N]),
    start(N-1, SeedPort, [P | Pids]).