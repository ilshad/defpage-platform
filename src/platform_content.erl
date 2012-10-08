-module(platform_content).

-export([content/1]).

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
	%io:format("Response: ~ts.\n", [unicode:characters_to_binary(Response)]),
	%{<<"title...">>, <<"body...">>};
	{ok, {{_, 404, _}, _, _}} ->
	    error_get_source;
	_ ->
	    error_get_source
    end.

rules(gd) ->
    [{key, parser:pack($*, $*)}].

parse(gd, Input) ->
    parse(gd, rules(gd), string:tokens(Input, "\n"), []).

parse(gd, Rules, [H|T], Acc) ->
    PKey = proplists:get_value(key, Rules),
    case PKey(H) of
	{[], Key} ->
	    parse(gd, Rules, T, Acc ++ [{key, Key}]);
	{[], []} ->
	    case lists:last(Acc) of
		{key, Key} ->
		    
