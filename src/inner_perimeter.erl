-module(inner_perimeter).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, [FirstIntersection|_] = Intersections) ->
    LastHex = connect2(Hexes, Intersections),
    LastHex ! {intersection, FirstIntersection}.

connect2([Hex|Hexes],[I1,I2|Intersections]) ->
    [Hex ! {intersection, I} || I <- [I1, I2]],
    connect3(Hexes, [I2|Intersections]).

connect3([LastHex], [I1,I2]) ->
    [LastHex ! {intersection, I} || I <- [I1, I2]],
    LastHex;
connect3([Hex|Hexes],[I1,I2,I3|Intersections]) ->
    [Hex ! {intersection, I} || I <- [I1, I2, I3]],
    connect2(Hexes, [I3|Intersections]).
