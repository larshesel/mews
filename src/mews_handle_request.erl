-module(mews_handle_request).
-export([handle_request/2]).

-include_lib("kernel/include/file.hrl").
-include("request.hrl").

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%%%===================================================================
%%% API
%%%===================================================================

handle_request(Socket, Request) -> 
    error_logger:info_msg("handle_request: Request: ~p~n:", [Request]),
    Rewritten = Request#request{uri = mews_rewrite:rewrite(Request#request.uri)},
    case Request#request.method of 
	get ->
	    handle_get_request(Socket, Rewritten);
	post -> 
	    handle_post_request(Socket, Rewritten);
	_ ->
	    error_logger:warning_msg("Request not supported: ~p~n:", [Rewritten]),
	    gen_tcp:send(Socket, build_header(status_501_not_implemented(), iolist_size(status_501_not_implemented_data()), "text/html"))
    end.


%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_post_request(Socket, Request) ->
    ok.

handle_get_request(Socket, Request) ->
    error_logger:info_msg("handle_get_request for uri: ~p~n", [Request#request.uri]),

    %% determine if uri can be served locally:
    case is_local_file(Request#request.uri) of 
	true ->
	    serve_local_file(Socket, Request);
	false -> 
	    redirect
    end.


%% local file, not redirect.
serve_local_file(Socket, Request) ->
    File = [get_webroot(), Request#request.uri], 
    case filelib:is_regular(File) of
	true ->  %% local file
	    error_logger:info_msg("is a regular file: ~p~n", [File]),
	    
	    case is_cgi(File) of 
		true -> process_cgi_file(Socket, Request, File);
		false ->
		    process_plain_file(Socket, File)
	    end;
	false -> 
	    error_logger:info_msg("not a regular file: ~p~n", [File]),
	    gen_tcp:send(Socket, build_header(status_404_not_found(), 
					      iolist_size(status_404_not_found_data()), "text/html")),
	    gen_tcp:send(Socket, status_404_not_found_data())
    end.

process_cgi_file(Socket, Request, File) ->
    %% spawn a separat process for executing the cgi script
    error_logger:info_msg("Processing cgi file ~p~n", [File]),
    spawn(fun() -> mews_cgi_executioner:execute_request(Socket, Request, File) end).

is_cgi(File) ->
    lists:suffix(".cgi", lists:flatten(File)).

process_plain_file(Socket, File) ->
	    {ok, FileInfo} = file:read_file_info(File),
	    gen_tcp:send(Socket, 
			 build_header(status_200_ok(), 
				      FileInfo#file_info.size, 
				      mime_types:get_mime_type(File))),

	    %% open file, and stream it to the socket
	    {ok, IoDevice} = file:open(File, [raw, read, binary]),
	    do_stream_file(Socket, IoDevice),
	    error_logger:info_msg("finished streaming content~n"),
	    file:close(IoDevice).

    
-define(FILE_READ_SIZE, 65536).

do_stream_file(Sock, IoDevice) -> 
    case file:read(IoDevice, ?FILE_READ_SIZE) of 

	{ok, Data} ->
	    gen_tcp:send(Sock, Data),
	    error_logger:info_msg("do_stream_file: sent chunk (size: ~p)!~n", [iolist_size(Data)]),
	    do_stream_file(Sock, IoDevice);
	eof -> 
	    error_logger:info_msg("do_stream_file: eof!~n"),
	    ok;
	{error, _Reason} ->
	    error_logger:info_msg("do_stream_file: {error, Reason}!~n"),
	    ok
    end.

status_501_not_implemented() ->
    ["HTTP/1.1 501 Not Implemented\r\n"].

status_501_not_implemented_data() ->
    ["<html><body><p>501 Not Implemented</p></body></html>\n"].

status_404_not_found() ->
    ["HTTP/1.1 404 Not Found\r\n"].

status_200_ok() ->
    ["HTTP/1.1 200 OK\r\n"].

status_404_not_found_data() ->
    ["<html><body><p>404 page not found</p></body></html>\n"].


is_local_file(_Uri) ->
    %% FIXME
    true.

build_header(Status, DataSize, Type) -> 
    [Status, 
     %%"Date: Mon, 23 May 2005 22:38:34 GMT\n", 
     get_server(), 
     %%"Last-Modified: Wed, 08 Jan 2003 23:11:55 GMT\n", 
     %%"Accept-Ranges: bytes\n",
     "Content-Length: ", integer_to_list(DataSize), "\n", 
     "Connection: close\n", 
     "Content-Type: ", Type, "; charset=UTF-8\n\n"].

get_server() ->
    "Server: MyErlangWebserver. Erlang-home-made-and-backed 0.01\n".

get_webroot() ->
    {ok, Dir} = application:get_env(webroot),
    Dir.



%%%===================================================================
%%% Internal unittests
%%%===================================================================

-ifdef(TEST).
is_cgi_test_() ->
    [?_assertEqual(true, is_cgi("testfile.cgi")),
     ?_assertEqual(false, is_cgi("testfile.cig"))].

-endif.
