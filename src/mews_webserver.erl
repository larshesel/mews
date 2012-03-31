-module(mews_webserver).
-export([listen/1, start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(SupCFG) -> 
    [{ip, _Ip},
     {port, Port}] = SupCFG,
    listen(Port).
    

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    do_accept(LSocket, 0).

do_accept(Lsocket, N) ->
    {ok, Socket} = gen_tcp:accept(Lsocket),
    spawn(fun() -> send_page(Socket) end), 
    error_logger:info_msg("Spawned socket number: ~p~n", [N]),
    do_accept(Lsocket, N + 1).

%%<<"GET / HTTP/1.1\r\nHost: localhost:8007\r\nConnection: keep-alive\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.83 Safari/535.11\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n\r\n">><<"GET /favicon.ico HTTP/1.1\r\nHost: localhost:8007\r\nConnection: keep-alive\r\nAccept: */*\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.83 Safari/535.11\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n\r\n">>

send_page(Socket) ->
    %% TODO: need to make sure that we have read the entire request... 
    case gen_tcp:recv(Socket, 0) of 
	{ok, Data} ->
	    error_logger:info_msg("Received data: ~p~n",[Data]),
	    RequestLines = string:tokens(binary_to_list(Data), "\r\n"),
	    {ok, ParsedRequest} = parse_request(RequestLines),
	    error_logger:info_msg("Parsed Request: ~p~n", [ParsedRequest]),
	    handle_request(Socket, ParsedRequest);
	{error, closed} ->
	    ok
    end.

handle_request(Socket, ParsedRequest) -> 
    {Request, Data} = ParsedRequest,
    error_logger:info_msg("handle_request: Request: ~p~n:", [ParsedRequest]),
    case Request of 
	get ->
	    handle_get_request(Socket, 0);
	_ ->
	    error_logger:warning_msg("Request not supported: ~p~n:", [ParsedRequest])
    end.


parse_request([RequestLine | _Tail]) ->
    error_logger:info_msg("Parse requestLine: ~p~n", [RequestLine]),
    %% parse the first line - that's enough for now.
    case parse_request_line(string:tokens(RequestLine, " ")) of 
    	{ok, Request} ->
    	    {ok, Request};
    	error ->
    	    {error, [parser_error, RequestLine]}
    end.

parse_request_line([FirstToken | Tail]) ->
    error_logger:info_msg("FirstToken: ~p~n", [FirstToken]),
    case FirstToken of 
	"GET" ->
	    [Uri, Version] = Tail,
	    {ok, {get, {{uri, Uri}, {version, Version}}}};
	_ -> error
    end.

handle_get_request(Socket, N) ->	
    Page=build_header(["Hello World: ", integer_to_list(N)]),
    gen_tcp:send(Socket, Page),
    gen_tcp:close(Socket).    

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
    "Server: MyErlangWebserver. Erlang-home-made-and-backed 0.01\n".

