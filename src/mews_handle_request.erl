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
	error_logger:info_msg("handle_get_request for uri: ~p~n", [Uri]),
	
	%% determine if uri can be served locally:
	case is_local_file(Uri) of 
		true ->
			serve_local_file(Socket, Uri);
		false ->	
			redirect
	end,
	gen_tcp:close(Socket).


%% local file, not redirect.
serve_local_file(Socket, Uri) ->
	File = [get_webroot(), Uri], 
	case filelib:is_regular(File) of
		true ->     
			{ok, Binary} = file:read_file(File),
			gen_tcp:send(Socket, build_header(status_200_ok(), Binary));
		false -> 
			gen_tcp:send(Socket, build_header(status_404_not_found(), status_404_not_found_data()))
	end.

status_404_not_found() ->
	["HTTP/1.1 404 Not Found\r\n"].

status_200_ok() ->
	["HTTP/1.1 404 Not Found\r\n"].

status_404_not_found_data() ->
	["<html><body><p>404 page not found</p></body></html>"].


is_local_file(_Uri) ->
	%% FIXME
	true.

build_header(Status, Data) -> 
	[Status, 
	 %%"Date: Mon, 23 May 2005 22:38:34 GMT\n", 
	 get_server(), 
	 %%"Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT\n", 
	 %%"Accept-Ranges: bytes\n",
	 "Content-Length: ", integer_to_list(iolist_size(Data)), "\n", 
	 "Connection: close\n", 
	 "Content-Type: text/html; charset=UTF-8\n\n", 
	 Data].

get_server() ->
	"Server: MyErlangWebserver. Erlang-home-made-and-backed 0.01\n".

get_webroot() ->
	"./test_data/webroot/".


