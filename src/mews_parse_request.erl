-module(mews_parse_request).
-export([parse_request/1]).

-include("request.hrl").

%%===================================================================
%%% API
%%%===================================================================

parse_request([RequestLine | _Tail]) ->
	error_logger:info_msg("Parse requestLine: ~p~n", [RequestLine]),
	%% parse the first line - that's enough for now.
	case string:tokens(RequestLine, " ") of 
		["HEAD", _Uri, _Version] -> {ok, {head, []}};
		["GET", Uri, Version] -> {ok, #request{method=get, uri=Uri, version=Version}};
		["POST", _Uri, _Version] -> {ok, {post, []}};
		["PUT", _Uri, _Version] -> {ok, {put, []}};
		["DELETE", _Uri, _Version] -> {ok, {delete, []}};
		["TRACE", _Uri, _Version] -> {ok, {trace, []}};
		["OPTIONS", _Uri, _Version] -> {ok, {options, []}};
		["CONNECT", _Uri, _Version] -> {ok, {connect, []}};
		["PATCH", _Uri, _Version] -> {ok, {patch, []}};
		_ -> {bad_request, bad_request()}
	end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

bad_request() ->
	[some_reason].
