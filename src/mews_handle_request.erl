-module(mews_handle_request).
-export([handle_request/2]).

handle_request(Socket, ParsedRequest) -> 
    {Request, _Data} = ParsedRequest,
    error_logger:info_msg("handle_request: Request: ~p~n:", [ParsedRequest]),
    case Request of 
	get ->
	    handle_get_request(Socket, 0);
	_ ->
	    error_logger:warning_msg("Request not supported: ~p~n:", [ParsedRequest])
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
     "Content-Length: ", integer_to_list(iolist_size(Data)), "\n", 
     "Connection: close\n", 
     "Content-Type: text/html; charset=UTF-8\n\n",
     Data].
    
get_server() ->
    "Server: MyErlangWebserver. Erlang-home-made-and-backed 0.01\n".
