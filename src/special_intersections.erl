-module(special_intersections).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, Intersections) ->
    lists:map(
        fun({intersection,IntersectionNum,hexes,HexNums}) ->
            Intersection = lists:nth(IntersectionNum+1, Intersections),
            [lists:nth(HexNum+1, Hexes) ! {intersection, Intersection} || HexNum <- HexNums]
        end, config()),
    true.

config() ->
    [{intersection,29,hexes,[11, 0]}].