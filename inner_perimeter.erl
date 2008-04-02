-module(inner_perimeter).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, Intersections) ->
    connect(2, [lists:last(Hexes)|Hexes], Intersections).

connect(_, [], _) -> done;
connect(0, [Hex|Hexes], [Intersection|Intersections]) ->
    Hex ! {intersection, Intersection},
    connect(2, Hexes, Intersections);
connect(N, [Hex1,Hex2|Hexes], [Intersection|Intersections]) ->
    Hex1 ! {intersection, Intersection},
    Hex2 ! {intersection, Intersection},
    connect(N-1, Hexes, Intersections).
