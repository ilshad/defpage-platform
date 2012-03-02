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
		   modified :: integer()}).

-record(source_doc, {title :: string(),
		     modified :: integer()}).

-spec(sync(Id::integer()) -> term()).
%% @doc Run sync process.
sync(CollectionId) ->
    {SourceType, MetaDocs} = get_meta(CollectionId),
    SourceDocs = get_sources(SourceType, CollectionId),
    lists:foreach(get_fun_update(SourceType, CollectionId, MetaDocs), SourceDocs),
    ok.

-spec(get_meta(CollectionId::integer()) -> {source_type(), [#meta_doc{}]}).
%% Get metadata info.
get_meta(CollectionId) ->
    Url = ?META_URL ++ "/collections/" ++ integer_to_list(CollectionId),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    Docs = proplists:get_value(<<"documents">>, Fields),
	    {source_type(Fields), [meta_doc(X) || X <- Docs]};
	{ok, {{_, 404, _}, _, _}} ->
	    error;
	_ ->
	    error
    end.

-spec(source_type(Fields::[tuple()]) -> source_type()).
%% Extract source type for collection. Assume alone.
source_type(Fields) ->
    [{struct, Source} | _] = proplists:get_value(<<"sources">>, Fields),
    case proplists:get_value(<<"type">>, Source) of
	<<"gd">> -> gd;
	_ -> error
    end.

-spec(get_sources(source_type(), CollectionId::integer()) -> [#source_doc{}]).
%% Get list of info about source documents.
get_sources(gd, CollectionId) ->
    Url = ?GD_URL ++ "/api/collection/" ++ integer_to_list(CollectionId) ++ "/documents",
    case httpc:request(Url) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [source_doc(X) || X <- mochijson2:decode(Body)];
	{ok, {{_, 404, _}, _, _}} ->
	    error;
	_ ->
	    error
    end.

%% Create property with record #meta_doc{} from json structure
meta_doc({struct, Fields}) ->
    {struct, Source} = mochijson2:decode(proplists:get_value(<<"source">>, Fields)), % ?!!
    {proplists:get_value(<<"id">>, Source),
     #meta_doc{meta_id = proplists:get_value(<<"id">>, Fields),
	       source_type = proplists:get_value(<<"type">>, Source),
	       title = proplists:get_value(<<"title">>, Fields),
	       modified = rfc3339:parse_epoch(
			    binary_to_list(
			      proplists:get_value(<<"modified">>, Fields)))}}.

%% Create property with record #source_doc{} from json structure
source_doc({struct, Fields}) ->
    {proplists:get_value(<<"id">>, Fields),
     #source_doc{title = proplists:get_value(<<"title">>, Fields),
		 modified = rfc3339:parse_epoch(
			      binary_to_list(
				proplists:get_value(<<"modified">>, Fields)))}}.

-spec(get_fun_update(SourceType::atom(),
		     CollectionId::integer(),
		     MetaDocs::list()) -> function()).
%% Return function which update meta docs if need, for given source document.
%% Check meta for corresponding document by given source.
%% If it does not found, then create new document.
%% If there is a corresponding document, then check `modified` and `title`
%% and update if need.
get_fun_update(SourceTypeAtom, CollectionId, MetaDocs) ->
    SourceType = list_to_binary(atom_to_list(SourceTypeAtom)),
    fun({SourceId, SourceDoc}) ->
	    case proplists:get_value(SourceId, MetaDocs) of
		undefined ->
		    create_doc(CollectionId, SourceType, SourceId, SourceDoc);
		{meta_doc, MetaId, SourceType, MetaTitle, MetaModified} ->
		    update_doc(MetaId, MetaTitle, MetaModified, SourceDoc)
	    end
    end.

-spec(create_doc(CollectionId::integer(),
		 SourceType::atom(),
		 SourceId::string(),
		 SourceDoc::term()) -> ok).
%% Create corresponding document in the metadata server for given source document.
create_doc(CollectionId, SourceType, SourceId, SourceDoc) ->
    Fields = {struct, [{<<"title">>, SourceDoc#source_doc.title},
		       {<<"source">>, {struct, [{<<"type">>, SourceType},
						{<<"id">>, SourceId}]}},
		       {<<"collection_id">>, CollectionId}]},

    Request = {?META_URL ++ "/documents/",
	       [?META_AUTH],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},

    case httpc:request(post, Request, [], []) of
	{ok, {{_, 201, _}, _, Body}} ->
	    {struct, ResponseFields} = mochijson2:decode(Body),
	    DocId = proplists:get_value(<<"id">>, ResponseFields),
	    io:format("Document ~p created.~n", [DocId]),
	    ok;
	_ ->
	    ok
    end.

-spec(update_doc(MetaId::string(),
		 MetaTitle::string(),
		 MetaModified::integer(),
		 SourceDoc::term()) -> ok).
%% Update corresponding meta document if `title` or `modified` is updated.
update_doc(MetaId, MetaTitle, MetaModified, SourceDoc) ->    
    if
	MetaTitle =/= SourceDoc#source_doc.title ->
	    io:format("~n[~p] found modified title~n", [MetaId]),
	    Fields = {struct, [{<<"title">>, SourceDoc#source_doc.title}]},
	    Request = {?META_URL ++ "/documents/" ++ integer_to_list(MetaId),
		       [?META_AUTH],
		       "application/json",
		       iolist_to_binary(mochijson2:encode(Fields))},
	    case httpc:request(post, Request, [], []) of
		{ok, {{_, 204, _}, _, _}} ->
		    io:format("[~p] updated successfully~n", [MetaId]);
		{ok, {{_, RespStatus, _}, _, _}} ->
		    io:format("[~p] error while upd title: ~p~n", [MetaId, RespStatus]);
		_ ->
		    io:format("[~p] unknown error while updating title~n", [MetaId])
	    end;
	MetaModified < SourceDoc#source_doc.modified ->
	    io:format("~n[~p] found modified content~n", [MetaId]),
	    Fields = {struct, [{<<"modified">>, true}]},
	    Request = {?META_URL ++ "/documents/" ++ integer_to_list(MetaId),
		       [?META_AUTH],
		       "application/json",
		       iolist_to_binary(mochijson2:encode(Fields))},
	    case httpc:request(post, Request, [], []) of
		{ok, {{_, 204, _}, _, _}} ->
		    io:format("[~p] updated successfully~n", [MetaId]);
		{ok, {{_, RespStatus, _}, _, _}} ->
		    io:format("[~p] error while upd title: ~p~n", [MetaId, RespStatus]);
		_ ->
		    io:format("[~p] unknown error while updating title~n", [MetaId])
	    end;
	true ->
	    ok
    end,
    {MetaId, MetaTitle, MetaModified, SourceDoc}.
