-module(parser).

%% API
-export([token/1,
	 until/1,
	 comb/1,
	 transform/2,
	 skip/1,
	 pack/2
	]).

token(V) ->
    fun(Input) ->
	    case lists:prefix(V, Input) of
		true ->
		    {Input--V, V};
		_ ->
		    fail
	    end
    end.

until(V) ->
    fun(Input) ->
	    Prefix = lists:takewhile(fun(X) -> X =/= V end, Input),
	    {Input -- Prefix, Prefix}
    end.

comb(Parsers) ->
    fun(Input) ->
	    lists:foldl(fun(P, {I, Acc}) ->
				case P(I) of
				    {Rest, V} ->
					{Rest, Acc ++ V};
				    fail ->
					{I, []}
				end
			end,
			{Input, []},
			Parsers)
    end.

transform(P, Fn) ->
    fun(Input) ->
	    case P(Input) of
		{Rest, V} ->
		    {Rest, Fn(V)};
		fail ->
		    fail
	    end
    end.

skip(P) ->
    transform(P, fun(_) -> [] end).

pack(L, R) ->
    comb([skip(token([L])),
	  until(R),
	  skip(token([R]))
	 ]).
