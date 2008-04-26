%% handles special case paths that are not covered by the
%% general walking algorithm
-module(special_paths).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([connect/2]).

connect(Intersections, Paths) ->
    {ok, SpecialPaths} = file:consult(special_paths),
    lists:map(
        fun({path,PathNum,from,A,to,B}) ->
            Path = lists:nth(PathNum, Paths),
            IntersectionA = lists:nth(A, Intersections),
            IntersectionB = lists:nth(B, Intersections),
            Path ! {intersection, IntersectionA},
            Path ! {intersection, IntersectionB},
            IntersectionA ! {path, Path},
            IntersectionA ! {intersection, IntersectionB},
            IntersectionB ! {path, Path},
            IntersectionB ! {intersection, IntersectionA}
        end, SpecialPaths),
    true.
