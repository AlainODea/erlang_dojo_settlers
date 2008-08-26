-module(walkable_paths).
-export([build/1]).

build([First|_] = Intersections) ->
    build1(First, Intersections).

build1(I2, [I1]) -> [path:between(I1, I2)];
build1(First, [I1,I2|Intersections]) ->
    [path:between(I1, I2)|build1(First, [I2|Intersections])].
