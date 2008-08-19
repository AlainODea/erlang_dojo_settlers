-module(walkable_paths).
-export([build/1]).

build([FirstIntersection|_] = Intersections) ->
    LastIntersection = build1(Intersections),
    Path = path:start(),
    LastIntersection ! {path, Path},
    FirstIntersection ! {path, Path}.

build1([LastIntersection]) -> LastIntersection;
build1([I1,I2|Intersections]) ->
    Path = path:start(),
    I1 ! {path, Path},
    I2 ! {path, Path},
    Path ! {intersection, I1},
    Path ! {intersection, I2},
    [Path|build([I2|Intersections])].
