-module(walkable_paths).
-export([build/1]).

build([_LastIntersection]) -> [];
build([I1,I2|Intersections]) ->
    Path = path:start(),
    I1 ! {path, Path},
    I2 ! {path, Path},
    Path ! {intersection, I1},
    Path ! {intersection, I2},
    [Path|build([I2|Intersections])].
