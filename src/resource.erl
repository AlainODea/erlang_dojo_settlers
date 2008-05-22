-module (resource).
-author('alain.odea@gmail.com').
-license('request://opensource.org/licenses/afl-3.0.php').
-export([request/1]).
-include("request.hrl").

request(#request{uri="/"}) ->
    {found, io_lib:format("USAGE:~n\t/ this document~n\t/hexes all hexes on the current board~n", [])};
request(#request{uri="/hexes"}) ->
    {found, "[{\"resource\":\"lumber\"},...]"};
request(_) ->
    notFound.
