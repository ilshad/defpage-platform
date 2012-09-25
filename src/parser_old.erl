-module(parser_old).

-export([]).
-export([string/1, assert/1, compose/1]).

string(S) ->
    fun(Input) ->
	    case lists:prefix(S, Input) of
		true ->
		    {S, Input--S};
		_ ->
		    fail
	    end
    end.

assert(P) ->
    fun(Input) ->
	    case P(Input) of
		fail ->
		     fail;
		_ ->
		    {[], Input}
	    end
    end.

compose(Parsers) ->
    fun(Input) ->
	    lists:foldl(fun(Fn, {Acc, I}) ->
				case Fn(I) of
				    {R, Rest} ->
					{Acc ++ R, Rest};
				    fail ->
					{[], I}
				end
			end,
			{[], Input},
			Parsers)
    end.
