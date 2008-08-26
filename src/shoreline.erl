-module(shoreline).
-export([connect/1]).

connect([_|Outers]) ->
    Null = spawn(fun() -> true end),
    connect1(Null, Outers).

connect1(_, []) -> true;
connect1(Null, [Outer|Outers]) ->
    connect(Outer, Null),
    connect2(2, Null, Outers).

connect2(0, Null, Outers) -> connect1(Null, Outers);
connect2(N, Null, [Outer,_|Outers]) ->
    connect(Outer, Null),
    connect2(N-1, Null, Outers);
connect2(1, Null, [Outer]) -> connect(Outer, Null).

connect(Outer, Null) ->
    Outer ! {path, Null},
    Outer ! {intersection, Null}.
