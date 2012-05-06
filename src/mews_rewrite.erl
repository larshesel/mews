-module(mews_rewrite).
-export([rewrite/1]).

-include("request.hrl").

rewrite(Rewrite) ->
    case Rewrite of 

	"/" ->
	    "/index.html";
	"" -> "index.html";
	_ -> Rewrite
	
    end.
