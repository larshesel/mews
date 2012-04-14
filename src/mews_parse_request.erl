-module(mews_parse_request).
-export([parse_request/1]).

-include("request.hrl").

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


bad_request() ->
	[some_reason].

%% keep this around for inspirational purposes ;)
%%<<"GET / HTTP/1.1\r\nHost: localhost:8007\r\nConnection: keep-alive\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.83 Safari/535.11\r\nAccept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n\r\n">><<"GET /favicon.ico HTTP/1.1\r\nHost: localhost:8007\r\nConnection: keep-alive\r\nAccept: */*\r\nUser-Agent: Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/535.11 (KHTML, like Gecko) Chrome/17.0.963.83 Safari/535.11\r\nAccept-Encoding: gzip,deflate,sdch\r\nAccept-Language: en-US,en;q=0.8\r\nAccept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n\r\n">>
