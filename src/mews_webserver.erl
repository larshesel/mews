%%%-------------------------------------------------------------------
%%% @author Lars Hesel Christensen <>
%%% @copyright (C) 2012, Lars Hesel Christensen
%%% @doc
%%%
%%% @end
%%% Created : 15 Apr 2012 by Lars Hesel Christensen <>
%%%-------------------------------------------------------------------
-module(mews_webserver).


-behaviour(gen_server).

%% API
-export([start/1, change_port/1, stop_accepting_requests/0, start_accepting_requests/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {cfg, listen_socket, webroot}).

%%%===================================================================
%%% API
%%%===================================================================


start(Cfg) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Cfg, []).

change_port(Port) ->
    gen_server:call(?MODULE, {change_port, Port}).

stop_accepting_requests() ->
    gen_server:call(?MODULE, {stop_accepting_requests}).

start_accepting_requests() ->
    gen_server:call(?MODULE, {start_accepting_requests}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Cfg) ->
    process_flag(trap_exit, true),
    Webroot = application:get_env(webroot),
    case Webroot of  
	undefined ->
	    {stop, {webroot, is_not_defined}};
	_ ->
	    {ok, Dir} = Webroot,
	    case filelib:is_dir(Dir) of 
		true ->
		    error_logger:info_msg("gen_server:init, Cfg = ~p~n", [Cfg]),
		    ListenSocket = listen(proplists:get_value(port, Cfg)),
		    {ok, #state{cfg=Cfg, listen_socket=ListenSocket}};
		false ->
		    {stop, {webroot, Webroot, is_not_a_valid_directory}}
	    end
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({change_port, NewPort}, _From, OldState) ->

    %% close the old listening socket
    gen_tcp:close(OldState#state.listen_socket),

    error_logger:info_msg("OldConfig: ~p~n", [OldState]),

    %% Update the configuration
    NewCfg = lists:map(fun({Name, Value}) -> 
			       case Name of 
				   port ->
				       {port, NewPort};
				   _ -> {Name, Value}
			       end
		       end,
		       OldState#state.cfg),
    error_logger:info_msg("NewConfig: ~p~n", [NewCfg]),

    %% start to listen on the new port.
    listen(proplists:get_value(port, NewCfg)),

    Reply = ok,
    {reply, Reply, OldState#state{cfg=NewCfg}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(Reason, _State) ->
    error_logger:info_msg("mews_webserver is being terminated: ~p~n", [Reason]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================



-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

listen(Port) ->
    case gen_tcp:listen(Port, ?TCP_OPTIONS) of 
	{ok, LSocket} -> 
	    spawn(fun() ->
			  do_accept(LSocket, 0)
		  end),
	    LSocket;
	{error, Reason} -> {error, Reason}
    end.

do_accept(Lsocket, N) ->
    case gen_tcp:accept(Lsocket) of 
	{ok, Socket} ->
	    spawn(fun() -> do_recv(Socket) end), 
	    error_logger:info_msg("Spawned socket number: ~p~n", [N]),
	    do_accept(Lsocket, N + 1);
	{error, closed} -> 
	    %% the listening socket was closed, just end the process.
	    ok
    end.

-define(READ_SOCKET_TIMEOUT, 60*1000).

do_recv(Socket) ->
    case gen_tcp:recv(Socket, 0, ?READ_SOCKET_TIMEOUT) of 
	{ok, Data} ->
	    error_logger:info_msg("Received data: ~p~n",[Data]),
	    RequestLines = string:tokens(binary_to_list(Data), "\r\n"),
	    %% TODO-LHC:
	    %%we got: Error in process <0.55.0> with exit value: {{badmatch,{bad_request,[some_reason]}},[{mews_webserver,do_recv,1,[{file,"src/mews_webserver.erl"},{line,191}]}]}
	    %% so we should not do this match - or if we do - we should act and return
	    %% a proper reply.
	    {ok, ParsedRequest} = mews_parse_request:parse_request(RequestLines),
	    error_logger:info_msg("Parsed Request: ~p~n", [ParsedRequest]),
	    mews_handle_request:handle_request(Socket, ParsedRequest),
	    do_recv(Socket);
	{error, timeout} ->
	    gen_tcp:close(Socket),
	    error_logger:info_msg("do_recv, socket closed, timout: ~p~n", [Socket]);
	{error, closed} ->
	    error_logger:info_msg("do_recv, socket closed: ~p~n", [Socket]),
	    ok
    end.
