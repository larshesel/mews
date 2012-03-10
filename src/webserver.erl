-module(webserver).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    do_accept(LSocket, 0).

do_accept(Lsocket, N) ->
    {ok, Socket} = gen_tcp:accept(Lsocket),
    spawn(fun() -> send_page(Socket, N) end), 
    do_accept(Lsocket, N + 1).


send_page(Socket, N) ->
    case gen_tcp:recv(Socket, 0) of 
	{ok, _Data} ->
	    Page=build_header(["Hello World: ", integer_to_list(N)]),
	    gen_tcp:send(Socket, lists:flatten(Page)),
	    gen_tcp:close(Socket);
	{error, closed} ->
	    ok
    end.

build_header(Data) -> 
    ["HTTP/1.1 200 OK\n", 
     "Date: Mon, 23 May 2005 22:38:34 GMT\n", 
     get_server(), 
     "Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT\n", 
     "Accept-Ranges: bytes\n",
     "Content-Length: ", integer_to_list(lists:flatlength(Data)), "\n", 
     "Connection: close\n", 
     "Content-Type: text/html; charset=UTF-8\n\n",
     Data].
    
get_server() ->
    "Server: Erlang-home-made-and-backed 0.01\n".

