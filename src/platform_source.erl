%% @copyright 2012 defpage.com
%% @doc Synchronize documents in metadata server with sources.

-module(platform_source).
-export([sync/1]).
-export([get_meta/1, get_sources/2]).

-include("platform.hrl").

-type(source_type() :: gd | undefined).

-record(meta_doc, {meta_id :: string(),
		   source_type :: string(),
		   title :: string(),
		   modified :: string()}).

-record(source_doc, {title :: string(),
		     modified :: string()}).

-spec(sync(Id::integer()) -> term()).
%% @doc Run sync process.
sync(CollectionId) ->
    {SourceType, MetaDocs} = get_meta(CollectionId),
    SourceDocs = get_sources(SourceType, CollectionId),
    ok.

-spec(get_meta(CollectionId::integer()) -> {source_type(), [#meta_doc{}]}).
%% Get metadata info.
get_meta(CollectionId) ->
    Url = ?META_URL ++ "/collections/" ++ integer_to_list(CollectionId),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson:decode(Body),
	    {array, Docs} = proplists:get_value("documents", Fields),
	    {source_type(Fields), [meta_doc(X) || X <- Docs]};
	{ok, {{_, 404, _}, _, _}} ->
	    error;
	_ ->
	    error
    end.

-spec(source_type(Fields::[tuple()]) -> source_type()).
%% Extract source type for collection. Assume alone.
source_type(Fields) ->
    {array, [{struct, Source} | _]} = proplists:get_value("sources", Fields),
    case proplists:get_value("type", Source) of
	"gd" -> gd;
	_ -> error
    end.

-spec(get_sources(source_type(), CollectionId::integer()) -> [#source_doc{}]).
%% Get list of info about source documents.
get_sources(gd, CollectionId) ->
    Url = ?GD_URL ++ "/api/collection/" ++ integer_to_list(CollectionId) ++ "/documents",
    case httpc:request(Url) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {array, Docs} = mochijson:decode(Body),
	    [source_doc(X) || X <- Docs];
	{ok, {{_, 404, _}, _, _}} ->
	    error;
	_ ->
	    error
    end.

meta_doc({struct, Fields}) ->
    {struct, Source} = proplists:get_value("source", Fields),
    {proplists:get_value("id", Source),
     #meta_doc{meta_id = proplists:get_value("id", Fields),
	       source_type = proplists:get_value("type", Source),
	       title = proplists:get_value("title", Fields),
	       modified = rfc3339:parse_epoch(proplists:get_value("modified", Fields))}}.

source_doc({struct, Fields}) ->
    {proplists:get_value("id", Fields),
     #source_doc{title = proplists:get_value("title", Fields),
		 modified = rfc3339:parse_epoch(proplists:get_value("modified", Fields))}}.
