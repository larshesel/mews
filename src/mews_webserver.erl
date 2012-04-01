-module(mews_webserver).
-export([listen/1, start/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

start(Cfg) -> 
    Port = proplists:get_value(port, Cfg),
    listen(Port).
    

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    do_accept(LSocket, 0).

do_accept(Lsocket, N) ->
    {ok, Socket} = gen_tcp:accept(Lsocket),
    spawn(fun() -> send_page(Socket) end), 
    error_logger:info_msg("Spawned socket number: ~p~n", [N]),
    do_accept(Lsocket, N + 1).

send_page(Socket) ->
    %% TODO: need to make sure that we have read the entire request... 
    case gen_tcp:recv(Socket, 0) of 
	{ok, Data} ->
	    error_logger:info_msg("Received data: ~p~n",[Data]),
	    RequestLines = string:tokens(binary_to_list(Data), "\r\n"),
	    {ok, ParsedRequest} = mews_parse_request:parse_request(RequestLines),
	    error_logger:info_msg("Parsed Request: ~p~n", [ParsedRequest]),
	    mews_handle_request:handle_request(Socket, ParsedRequest);
	{error, closed} ->
	    ok
    end.
