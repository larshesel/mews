-module(mews_cgi_executioner).

-export([execute_request/2]).



execute_request(Socket, _File) ->
    error_logger:info_msg("Processing cgi script"),
    gen_tcp:close(Socket).

