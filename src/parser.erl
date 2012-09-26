-module(parser).

-export([succeed/1,
	 lambda/1,
	 satisfy/1,
	 symbol/1,
	 token/1,
	 fail/1,
	 then/2,
	 sum/2,
	 transform/2,
	 spaces/1,
	 just/1,
	 some/1,
	 then_fst/2,
	 then_snd/2
	]).

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

transform(Fn, P) ->
    fun(Input) ->
	    [{Rest, Fn(X)} || {Rest, X} <- P(Input)]
    end.

spaces(P) ->
    fun(Input) ->
	    P(lists:dropwhile(fun(I) -> I == 32 end, Input))
    end.

just(P) ->
    fun(Input) ->
	    [{A, B} || {A, B} <- P(Input), A == []]
    end.

some(P) ->
    fun(Input) ->
	    Fn = just(P),
	    [{_, X} | _] = Fn(Input),
	    X
    end.

%% Transformed parser combinators

then_fst(P1, P2) ->
    transform(fun({X, _}) -> X end, then(P1, P2)).

then_snd(P1, P2) ->
    transform(fun({_, X}) -> X end, then(P1, P2)).
