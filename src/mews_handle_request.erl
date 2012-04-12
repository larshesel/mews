-module(mews_handle_request).
-export([handle_request/2]).

handle_request(Socket, ParsedRequest) -> 
    {Request, Data} = ParsedRequest,
    error_logger:info_msg("handle_request: Request: ~p~n:", [ParsedRequest]),
    case Request of 
	get ->
	    handle_get_request(Socket, Data);
	_ ->
	    error_logger:warning_msg("Request not supported: ~p~n:", [ParsedRequest])
    end.

handle_get_request(Socket, Data) ->
    Uri = proplists:get_value(uri, Data),
    error_logger:info_msg("handle_get_request: reading file: ~p~n", [Uri]),
    File = "./test_data/webroot/index.html", 
    case filelib:is_regular(File) of
	true ->     
	    {ok, Binary} = file:read_file("./test_data/webroot/index.html"),
	    gen_tcp:send(Socket, build_header(Binary));
	false-> ok
    end,
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


