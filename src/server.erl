-module(server).
-author('alain.odea@gmail.com').
-license('request://opensource.org/licenses/afl-3.0.php').
-export([start/0]).
-include("request.hrl").

start() ->
    {ok, Listen} = gen_tcp:listen(8008, [list, {packet, line},
                                         {reuseaddr, true},
                                         {active, true}]),
    spawn(fun() -> par_connect(Listen) end).

respond(Socket, Status, Response)
        when is_list(Status), is_list(Response), length(Response) =/= 0 ->
    gen_tcp:send(Socket, io_lib:format("HTTP/1.1 ~s\r\n", [Status])),
    gen_tcp:send(Socket, io_lib:format("Content-Length: ~p\r\n", [length(Response)])),
    gen_tcp:send(Socket, "Content-Type: text/plain\r\n"),
    gen_tcp:send(Socket, "Set-Cookie: session=1; path=/; domain=.familyodea.com\r\n"),
    gen_tcp:send(Socket, "\r\n"),
    gen_tcp:send(Socket, Response);
respond(Socket, Status, _) when is_list(Status) ->
    gen_tcp:send(Socket, io_lib:format("HTTP/1.1 ~s\r\n", [Status])),
    gen_tcp:send(Socket, "\r\n").

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    io:format("Client session started~n"),
    spawn(fun() -> par_connect(Listen) end),
    Next = fun() ->
        receive
            {tcp, Socket, Data} ->
                {ok, Line, _} = regexp:gsub(Data, "[\r\n]", ""),
                Line;
            {tcp_closed, Socket} ->
                io:format("Client unceremoniously dropped request~n"),
                exit(closed)
        end
    end,
    Respond = fun
    (ok, Response) ->         respond(Socket, "200 OK", Response);
    (badRequest, Response) -> respond(Socket, "400 Bad Request", Response);
    (notFound, Response) ->   respond(Socket, "400 Not Found", Response)
    end,
    Req = #request{next=Next, respond=Respond},
    try
        waiting(Req)
    catch
    _:Msg ->
        Stack = erlang:get_stacktrace(),
        io:format("~s~n~p~n", [Msg, Stack]),
        respond(Socket, "500 Internal Server Error", Msg),
        erlang:raise({Msg, Stack})
    end.

waiting(Req) ->
    request_line(Req, regexp:split((Req#request.next)(), $ )).

request_line(Req, {ok, [Method, URI, Protocol]}) ->
    headers_nohost(Req#request{method=Method,uri=URI, protocol=Protocol},
        regexp:split((Req#request.next)(), ":\s+"));
request_line(Req, _) ->
    io:format("Malformed request:  bad request line~n"),
    (Req#request.respond)(badRequest, "Bad request line\r\n").

headers_nohost(#request{respond=Respond}, {ok, [[]]}) ->
    io:format("Malformed request: missing Host header~n"),
    Respond(badRequest, "Missing Host header\r\n");
headers_nohost(#request{headers=Headers, next=Next} = Req, {ok, ["Host", Host]}) ->
    headers(Req#request{headers=[{"Host", Host}|Headers]},
        regexp:split(Next(), ":\s*"));
headers_nohost(#request{headers=Headers, next=Next}=Req, {ok, [Name, Value]}) ->
    headers_nohost(Req#request{headers=[{Name, Value}|Headers]},
        regexp:split(Next(), ":\s*"));
headers_nohost(#request{respond=Respond}, BadFormat) ->
    io:format("Could not understand ~s~n", [BadFormat]),
    Respond(badRequest, "Could not understand header line\r\n").

headers(Req, {ok, [[]]}) ->
    #request{uri=URI, headers=Headers, method=Method} = Req,
    io:format("Request: URI:~p Headers:~p Method:~p~n", [
        URI, Headers, Method]),
    io:format("Request done, responding~n"),
    case resource:request(Req) of
    {found, Resource} ->
        (Req#request.respond)(ok, Resource);
    _ ->
        (Req#request.respond)(notFound, null)
    end;
headers(Req, {ok, [Name, Value]}) ->
    headers(Req#request{headers=[{Name, Value}|Req#request.headers]},
        regexp:split((Req#request.next)(), ":\s+"));
headers(#request{respond=Respond}, BadFormat) ->
    io:format("Could not understand ~s~n", [BadFormat]),
    Respond(badRequest, "Could not understand header line\r\n").