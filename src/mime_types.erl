-module(mime_types).

-export([get_mime_type/1]).



%%--------------------------------------------------------------------
%% @doc return the mime-type of the file, based on the file extension
%% @spec 
%% @end
%%--------------------------------------------------------------------
get_mime_type(FileName) ->
    Extension = filename:extension(FileName),
    case string:to_lower(Extension) of 
	".jpg" ->  "image/jpeg";
	".jpeg" -> "image/jpeg";
	".html" ->  "text/html";
	".htm" -> "text/html";
	".ico" ->  "image/x-icon";
	_ -> {error, unknown}
    end.
