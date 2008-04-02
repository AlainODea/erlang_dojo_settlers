-module(project).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([make/0, cover/0]).

each(Fun) ->
    lists:map(Fun, [actor, escrow, game, hex, inner_perimeter, 
                    intersection, outer_perimeter, path, player,
                    server, status, stockpile, test_game]).

make() ->
    each(fun(Mod) -> shell_default:c(Mod, [debug_info]) end).

cover() ->
    cover:start(),
    each(fun cover:compile/1),
    catch test_game:run(),
    each(fun cover:analyse_to_file/1).
