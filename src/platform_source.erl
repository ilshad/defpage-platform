%% @copyright 2012 defpage.com
%% @doc Synchronize documents in metadata server with sources.

-module(platform_source).
-export([sync/1, google_docs/1]).

-include("platform.hrl").

-record(document, {id::string(), title::string(), modified::string()}).

-spec(sync(Id::integer()) -> ok).

%% @doc Run synchronization process. Argument is collection Id.
sync(Id) -> Id.

-spec(google_docs(Id::integer()) -> [#document{}]).

%% @doc Get google docs for given collection. Argument is collection Id.
google_docs(Id) ->
    Url = ?GD_URL ++ "/api/collection/" ++ integer_to_list(Id) ++ "/documents",
    case httpc:request(Url) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {array, Docs} = mochijson:decode(Body),
	    source_documents(Docs);
	{ok, {{_, 404, _}, _, _}} -> error;
	_ -> error
    end.

source_documents(List) ->
    source_documents(List, []).

source_documents([], Acc) ->
    Acc;
source_documents([{struct, Fields} | Tail], Acc) ->
    X = #document{id = proplists:get_value("id", Fields),
		  title = proplists:get_value("title", Fields),
		  modified = proplists:get_value("modified", Fields)},
    source_documents(Tail, [X | Acc]).
