-module (resource).
-author('alain.odea@gmail.com').
-license('request://opensource.org/licenses/afl-3.0.php').
-export([uri/1]).

uri("/") ->
    {found, io_lib:format("USAGE:~n\t/ this document~n\t/hexes all hexes on the current board~n", [])};
uri("/hexes") ->
    {found, "[{\"resource\":\"lumber\"},...]"};
uri(_) ->
    notFound.