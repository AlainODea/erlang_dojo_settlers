-module(test_stockpile).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([run/0]).

run() ->
    [testProduction(N) || N <- lists:seq(1, 10)],
    true.

assert(Actor, Status) ->
    case status:check(Actor) of
    Status -> true;
    BadStatus -> throw({expect, Status, was, BadStatus})
    end.

testProduction(N) ->
    testLumber(N),
    testBrick(N),
    testGrain(N),
    testWool(N),
    testOre(N).

testLumber(N) ->
    Stockpile = stockpile:new(),
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}]}),
    Stockpile ! {produce, lumber, N},
    assert(Stockpile, {stockpile, [{lumber, N}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}]}).

testBrick(N) ->
    Stockpile = stockpile:new(),
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}]}),
    Stockpile ! {produce, brick, N},
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, N}, {grain, 0}, {wool, 0}, {ore, 0}]}).

testGrain(N) ->
    Stockpile = stockpile:new(),
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}]}),
    Stockpile ! {produce, grain, N},
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, N}, {wool, 0}, {ore, 0}]}).

testWool(N) ->
    Stockpile = stockpile:new(),
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}]}),
    Stockpile ! {produce, wool, N},
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, 0}, {wool, N}, {ore, 0}]}).

testOre(N) ->
    Stockpile = stockpile:new(),
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}]}),
    Stockpile ! {produce, ore, N},
    assert(Stockpile, {stockpile, [{lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, N}]}).
