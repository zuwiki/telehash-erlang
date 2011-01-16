-module(teletest).

-include("../include/telehash.hrl").

-export([start/1]).

start(0) -> ok;
start(N) ->
    telehash:start_link([9000+N]),
    test(N-1, 9000+N).

start(0, _SeedPort) -> ok;
start(N, SeedPort) ->
    telehash:start_link([{{127,0,0,1}, SeedPort}, 9000+N]),
    test(N-1, SeedPort).