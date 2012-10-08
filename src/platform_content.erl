-module(platform_content).

-export([content/1]).

-export([parse/1]).

-include("platform.hrl").

content(DocId) ->
    {SourceType, CollectionId, UID} = document(DocId),
    content(SourceType, CollectionId, UID).

document(DocId) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(DocId),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    {struct, Source} = proplists:get_value(<<"source">>, Fields),

	    {platform_source:source_type(Fields),
	     proplists:get_value(<<"collection_id">>, Fields),
	     binary_to_list(proplists:get_value(<<"id">>, Source))};

	_ -> error
    end.

content(gd, CollectionId, UID) ->
    Url = ?GD_URL
	++ "/api/collection/" ++ integer_to_list(CollectionId)
	++ "/documents/" ++ UID,
    case httpc:request(Url) of
	{ok, {{_, 200, _}, _, Response}} ->
	    Response;
	{ok, {{_, 404, _}, _, _}} ->
	    error_get_source;
	_ ->
	    error_get_source
    end.

pkey([H|T]) ->
    case (H == 42) and (lists:last(T) == 42) of
	true ->
	    {ok, string:strip(T, right, 42)};
	_ ->
	    fail
    end.

parse(S) ->
    parse(string:tokens(S, "\n"), []).

parse([H|T], Acc) ->
    case pkey(H) of
	{ok, Key} ->
	    parse(T, Acc, {Key, []});
	fail ->
	    fail
    end.

parse([H|T], Acc, {K,V}) ->
    case pkey(H) of
	{ok, Key} ->
	    parse(T, Acc ++ [{K,V}], {Key, []});
	fail ->
	    parse(T, Acc, {K, V ++ H})
    end;
parse([], Acc, {K,V}) ->
    Acc ++ [{K,V}].
