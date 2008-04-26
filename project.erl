-module(project).
-author('alain.odea@gmail.com').
-license('http://opensource.org/licenses/afl-3.0.php').
-export([make/0, cover/0]).

each(Dir, Fun) ->
    [perform(Fun, FileName) || FileName <- filelib:wildcard ("*.erl", atom_to_list(Dir))].

perform(Fun, FileName) ->
    Mod = list_to_atom(string:substr(FileName, 1, length(FileName) - 4)),
    Fun(Mod).

reload(Mod) ->
    c:c(Mod, [debug_info]).

make() ->
    c:cd(src),
    each(src, fun reload/1),
    c:cd('..'),
    c:cd(test),
    each(test, fun reload/1),
    c:cd('..').

cover() ->
    cover:start(),
    c:cd(src),
    each(src, fun cover:compile/1),
    c:cd('..'),
    c:cd(test),
    each(test, fun reload/1),
    c:cd('..'),
    each(test, fun run/1),
    c:cd(cover),
    each(src, fun cover:analyse_to_file/1),
    c:cd('..').

run(Mod) ->
    catch Mod:run().
