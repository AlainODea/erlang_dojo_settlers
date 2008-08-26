-module(middle_perimeter).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, [FirstIntersection|_] = Intersections) ->
    LastHex = connect4(Hexes, Intersections),
    LastHex ! {intersection, FirstIntersection}.

connect4([LastHex],[I1,I2,I3]) ->
    [LastHex ! {intersection, I} || I <- [I1, I2, I3]],
    LastHex;
connect4([Hex|Hexes],[I1,I2,I3,I4|Intersections]) ->
    [Hex ! {intersection, I} || I <- [I1, I2, I3, I4]],
    connect4(Hexes, [I4|Intersections]).
