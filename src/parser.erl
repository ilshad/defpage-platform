-module(parser).

-export([]).
-export([]).

%% Basic parser generators

succeed(V) ->
    fun(Input) -> [{Input, V}] end.

lambda(_) -> succeed({}).

satisfy(P) ->
    fun(Input) -> satisfy(P, Input) end.

satisfy(_, []) -> [];
satisfy(P, [H|T]) ->
    case P(H) of
	true -> [{T, H}];
	_ -> []
    end.

symbol(V) ->
    satisfy(fun(Ch) -> lists:equal(V, Ch) end).

token(K) ->
    N = length(K),
    fun(Input) ->
	    case K == lists:nth(N, Input) of
		true -> [{lists:nthtail(N, Input), K}];
		_ -> []
	    end
    end.

fail(_) ->
    fun(_) -> [] end.

%% Parser combinators

and_p(Parsers) ->
    fun(Input) ->
	    {Rest, Result} = lists:foldl(fun(Fn, {I, Acc}) ->
						 {Rest, X} = Fn(I),
						 {Rest, Acc + X}
					 end,
					 {Input, []},
					 Parsers),
	    [{Rest, list_to_tuple(Result)}]
    end.

or_p(Parsers) ->
    fun(Input) ->
	    lists:map(fun(Fn) -> Fn(Input) end, Parsers)
    end.

%% Parser transformers

transform(Fn, Parser) ->
    fun(Input) ->
	    [{Rest, Fn(X)} || {Rest, X} <- Parser(Input)]
    end.

%% Custom parsers
