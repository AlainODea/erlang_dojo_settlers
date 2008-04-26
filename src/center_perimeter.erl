-module(center_perimeter).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, Intersections) ->
    connect1([lists:last(Hexes)|Hexes], Intersections).

connect1([Hex1,Hex2|Hexes], [Intersection|Intersections]) ->
    Hex1 ! {intersection, Intersection},
    Hex2 ! {intersection, Intersection},
    connect1(Hexes, Intersections).
