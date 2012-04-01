-module(http_requests_tests).
-include_lib("eunit/include/eunit.hrl").

get_bad_request_test() ->
    {bad_request, _ } = mews_parse_request:parse_request(["blahblah"]).

%% this will be invalid, when we start checking if Uri and Version is supported
accept_get_request_test() ->
    {ok, _} = mews_parse_request:parse_request(["GET / HTTP/1.1"]).
    
