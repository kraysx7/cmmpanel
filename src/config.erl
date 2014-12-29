-module(config).
-author("Ilya Troshkov").
-export([get/1]).


get(Par) ->
    case application:get_env(Par) of
		{ok, Val} ->
		    Val;
		undefined ->
		    ""
	    end.
