-module(parser).

-export([succeed/1,
	 lambda/1,
	 satisfy/1,
	 symbol/1,
	 token/1,
	 fail/1,
	 then/2,
	 sum/2,
	 transform/2]).

%% Basic parsers

succeed(V) ->
    fun(Input) -> [{Input, V}] end.

lambda(_) ->
    succeed({}).

satisfy(P) ->
    fun(Input) -> satisfy(P, Input) end.

satisfy(_, []) -> [];
satisfy(P, [H|T]) ->
    case P(H) of
	true -> [{T, H}];
	_ -> []
    end.

symbol(V) ->
    satisfy(fun(Ch) -> string:equal(V, Ch) end).

token(V) ->
    fun(Input) ->
	    case lists:prefix(V, Input) of
		true -> [{lists:nthtail(length(V), Input), V}];
		_ -> []
	    end
    end.

fail(_) ->
    fun(_) -> [] end.

%% Parser combinators

then(P1, P2) ->
    fun(Input) ->
	    [{B, {V1, V2}} || {A, V1} <- P1(Input),
			      {B, V2} <- P2(A)]
    end.

sum(P1, P2) ->
    fun(Input) ->
	    P1(Input) ++ P2(Input)
    end.

%% Parser transformers

transform(Fn, Parser) ->
    fun(Input) ->
	    [{Rest, Fn(X)} || {Rest, X} <- Parser(Input)]
    end.

%% Custom parsers
