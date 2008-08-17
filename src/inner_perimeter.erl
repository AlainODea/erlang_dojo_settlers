-module(inner_perimeter).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, [FirstIntersection|_] = Intersections) ->
    LastHex = connecteven(Hexes, Intersections),
    LastHex ! {intersection, FirstIntersection}.

% an even hex needs its inner edges connected to two intersections
connecteven([Hex|Hexes], [I1,I2|Intersections]) ->
    Hex ! {intersection, I1},
    Hex ! {intersection, I2},
    connectodd(Hexes, [I2|Intersections]).

% the last hex needs to be connected to three intersections,
% but the third intersection here is the first intersection overall
connectodd([Hex], [I1,I2|_]) ->
    Hex ! {intersection, I1},
    Hex ! {intersection, I2},
    Hex;
% an odd hex needs its inner edges connected to three intersections
connectodd([Hex|Hexes], [I1,I2,I3|Intersections]) ->
    Hex ! {intersection, I1},
    Hex ! {intersection, I2},
    Hex ! {intersection, I3},
    connecteven(Hexes, [I3|Intersections]).
