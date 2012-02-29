%% @copyright 2012 defpage.com
%% @doc Synchronize documents in metadata server with sources.

-module(platform_source).
-export([sync/1, google_docs/1]).

-include("platform.hrl").

-record(document, {id::string(), title::string(), modified::string()}).

-spec(sync(Id::integer()) -> ok).

%% @doc Run sync process.
sync(CollectionId) -> CollectionId.

-spec(google_docs(CollectionId::integer()) -> [#document{}]).

%% @doc Get google docs for given collection. Argument is collection Id.
google_docs(CollectionId) ->
    Url = ?GD_URL ++ "/api/collection/" ++ integer_to_list(CollectionId) ++ "/documents",
    case httpc:request(Url) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {array, Docs} = mochijson:decode(Body),
	    [json_to_document(X) || X <- Docs];
	{ok, {{_, 404, _}, _, _}} -> error;
	_ -> error
    end.

json_to_document({struct, Fields}) ->
    #document{id = proplists:get_value("id", Fields),
	      title = proplists:get_value("title", Fields),
	      modified = rfc3339:parse(proplists:get_value("modified", Fields))}.
