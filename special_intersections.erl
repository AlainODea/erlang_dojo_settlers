-module(special_intersections).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Hexes, Intersections) ->
    {ok, SpecialIntersections} = file:consult(special_intersections),
    lists:map(
        fun({intersection,IntersectionNum,hexes,HexNums}) ->
            Intersection = lists:nth(IntersectionNum, Intersections),
            lists:map(
                fun(HexNum) ->
                    lists:nth(HexNum, Hexes) ! {intersection, Intersection}
                end, HexNums)
        end, SpecialIntersections),
    true.
