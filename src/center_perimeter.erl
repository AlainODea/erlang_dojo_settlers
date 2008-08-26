-module(center_perimeter).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, [FirstIntersection|_] = Intersections) ->
    LastHex = connect1(Hexes, Intersections),
    LastHex ! {intersection, FirstIntersection}.

connect1([LastHex], [LastIntersection]) ->
    LastHex ! {intersection, LastIntersection},
    LastHex;
connect1([Hex|Hexes], [I1,I2|Intersections]) ->
    Hex ! {intersection, I1},
    Hex ! {intersection, I2},
    connect1(Hexes, [I2|Intersections]).
