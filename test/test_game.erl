-module(test_game).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([run/0]).

run() ->
    test().

test() ->
    Hex = hex:start(),
    Stockpile = stockpile:start(),
    Paths = make_n(path, 6),,
    Intersections = make_n(intersection, 6),
    OpenReceiver = spawn(fun() -> true end),
    test(Hex, Stockpile, Paths, Intersctions, OpenReceiver).

test(Hex, Stockpile, Paths, Intersections, OpenReceiver) ->
    testInitialIntersectionState(Hex, Stockpile, Paths, Intersections, OpenReceiver).

testInitialIntersectionState(Hex, Stockpile, Paths, Intersections, OpenReceiver) ->
    [assert(Intersection, {waiting, for, [{path, 3}, {intersection, 3}]}) ||
        Intersection <- Intersections],
    testHookUpPathsAndIntersections(Hex, Stockpile, Paths, Intersections, OpenReceiver).

testHookUpPathsAndIntersections(Hex, Stockpile, Paths, Intersections, OpenReceiver) ->
    lists:foldl(
        fun(Path, [AnIntersection1,AnIntersection2|AnIntersections]) ->
            assert(Path, {waiting, for, [{intersection, 2}]}),
            Path ! {intersection, AnIntersection1},
            AnIntersection1 ! {path, Path},
            AnIntersection1 ! {intersection, AnIntersection2},
            assert(Path, {waiting, for, [{intersection, 1}]}),
            Path ! {intersection, AnIntersection2},
            AnIntersection2 ! {path, Path},
            AnIntersection2 ! {intersection, AnIntersection1},
            assert(Path, disconnected),
            [AnIntersection2|AnIntersections];
        (Path, [AnIntersection1]) ->
            [AnIntersection2|_] = Intersections,
            Path ! {intersection, AnIntersection1},
            AnIntersection1 ! {path, Path},
            AnIntersection1 ! {intersection, AnIntersection2},
            assert(Path, {waiting, for, [{intersection, 1}]}),
            Path ! {intersection, AnIntersection2},
            AnIntersection2 ! {path, Path},
            AnIntersection2 ! {intersection, AnIntersection1},
            assert(Path, disconnected),
            []
        end,
        Intersections,
        Paths),
    testHookUpHexAndIntersections(Hex, Stockpile, Paths, Intersections, OpenReceiver).

testHookUpHexAndIntersections(Hex, Stockpile, Paths, Intersections, OpenReceiver) ->
    assert(Hex, {waiting, for, [{intersections, 6}]}),
    lists:map(
        fun(Intersection) ->
            assert(Intersection, {waiting, for,
                [{path, 1}, {intersection, 1}]}),
            Intersection ! {path, OpenReceiver},
            assert(Intersection, {waiting, for, [{intersection, 1}]}),
            Intersection ! {intersection, OpenReceiver},
            assert(Intersection, disconnected),
            Hex ! {intersection, Intersection}
        end, Intersections),
    {hex, _} = status:check(Hex),
    testProductionAndBuilding(Hex, Stockpile, Paths, Intersections, OpenReceiver).

testProductionAndBuilding(Hex, Stockpile, Paths, Intersections, OpenReceiver) ->
    Hex ! produce,
    [Intersection1,_,Intersection3|_] = Intersections,
    assert(Stockpile, {stockpile, [
        {lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}
    ]}),
    Stockpile ! {produce, lumber, 1},
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}
    ]}),
    Stockpile ! {produce, brick, 1},
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 1}, {grain, 0}, {wool, 0}, {ore, 0}
    ]}),
    Stockpile ! {produce, grain, 1},
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 1}, {grain, 1}, {wool, 0}, {ore, 0}
    ]}),
    Stockpile ! {produce, wool, 1},
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 1}, {grain, 1}, {wool, 1}, {ore, 0}
    ]}),
    Intersection1 ! {basecamp, Stockpile},
    assert(Intersection1, {settlement, Stockpile}),
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 1}, {grain, 1}, {wool, 1}, {ore, 0}
    ]}),
    assert(Intersection3, disconnected),
    Intersection3 ! {settlement, Stockpile},
    assert(Intersection3, disconnected),
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 1}, {grain, 1}, {wool, 1}, {ore, 0}
    ]}),
    [Path1, Path2|_] = Paths,
    assert(Path1, connected),
    Path1 ! {road, Stockpile},
    assert(Path1, {road, Stockpile}),
    assert(Stockpile, {stockpile, [
        {lumber, 0}, {brick, 0}, {grain, 1}, {wool, 1}, {ore, 0}
    ]}),
    Stockpile ! {produce, lumber, 1},
    Stockpile ! {produce, brick, 1},
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 1}, {grain, 1}, {wool, 1}, {ore, 0}
    ]}),
    assert(Path2, connected),
    Path2 ! {road, Stockpile},
    assert(Path2, {road, Stockpile}),
    assert(Stockpile, {stockpile, [
        {lumber, 0}, {brick, 0}, {grain, 1}, {wool, 1}, {ore, 0}
    ]}),
    Stockpile ! {produce, lumber, 1},
    Stockpile ! {produce, brick, 1},
    assert(Stockpile, {stockpile, [
        {lumber, 1}, {brick, 1}, {grain, 1}, {wool, 1}, {ore, 0}
    ]}),
    assert(Intersection3, connected),
    Intersection3 ! {settlement, Stockpile},
    assert(Intersection3, {settlement, Stockpile}),
    assert(Stockpile, {stockpile, [
        {lumber, 0}, {brick, 0}, {grain, 0}, {wool, 0}, {ore, 0}
    ]}),
    allTestsPass(Hex, Stockpile, Paths, Intersections, OpenReceiver).

allTestsPass(_Hex, _Stockpile, _Paths, _Intersections, _OpenReceiver) ->
    true.

assert(Actor, Status) ->
    case status:check(Actor) of
    Status -> true;
    BadStatus -> throw({expect, Status, was, BadStatus})
    end.

make_n(Actor, N) -> [Actor:start() || _ <- lists:seq(1, N)].
