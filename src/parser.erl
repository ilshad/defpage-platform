-module(parser).

%% API
-export([gd_attrs/0]).

%% testing
-export([succeed/1,
	 lambda/1,
	 satisfy/1,
	 symbol/1,
	 token/1,
	 assert/1,
	 fail/1,
	 comb/1,
	 transform/2,
	 transform2/2,
	 spaces/1,
	 just/1,
	 zero_or_many/1,
	 single_or_many/1,
	 option/1
	]).

%% Basic parsers

succeed(V) ->
    fun(Input) -> {Input, V} end.

lambda(_) ->
    succeed([]).

satisfy(P) ->
    fun(Input) -> satisfy(P, Input) end.

satisfy(_, []) ->           fail;
satisfy(P, [H|T]) ->        satisfy(P(H), T, H).

satisfy(true, Rest, V) ->   {Rest, V};
satisfy(_, _, _) ->         fail.

symbol(V) ->
    satisfy(fun(Ch) -> string:equal(V, Ch) end).

token(V) ->
    fun(Input) ->
	    case lists:prefix(V, Input) of
		true -> {Input--V, V};
		_ -> fail
	    end
    end.

fail(_) ->
    fun(_) -> fail end.

%% Combinators

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

%% Transformers

transform(P, Fn) ->
    fun(Input) ->
	    {Rest, V} = P(Input),
	    {Rest, Fn(V)}
    end.

transform2(P, Fn) ->
    fun(Input) ->
	    {Rest, V} = P(Input),
	    Fn({Rest, V})
    end.

spaces(P) ->
    fun(Input) ->
	    P(lists:dropwhile(fun(I) -> I == 32 end, Input))
    end.

just(P) ->
    fun(Input) ->
	    case P(Input) of
		{[], V} -> {[], V};
		_ -> fail
	    end
    end.

assert(P) ->
    fun(Input) ->
	    case P(Input) of
		fail -> fail;
		_ -> {Input, []}
	    end
    end.

%% Repetion and option

zero_or_many(P) ->
    fun(Input) ->
	    zero_or_many(P, Input, [])
    end.

zero_or_many(P, Input, Acc) ->
    case P(Input) of
	{Rest, V} ->
	    zero_or_many(P, Rest, Acc ++ V);
	fail ->
	    {Input, Acc}
    end.

single_or_many(P) ->
    transform2(zero_or_many(P),
	       fun({Rest, V}) ->
		       case V of
			   [] ->
			       fail;
			   _ ->
			       {Rest, V}
		       end
	       end).

option(P) ->
    fun(Input) ->
	    case P(Input) of
		{Rest, V} ->
		    {Rest, V};
		fail ->
		    {Input, []}
	    end
    end.

%%%%%%%%%% Custom parsers %%%%%%%%%%

gd_attrs() ->
    zero_or_many(gd_attr).
    
