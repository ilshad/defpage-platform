%%% Synchronize documents in metadata server with sources.

-module(platform_source).

%% API
-export([sync/1, source_type/1]).

%% testing
-export([]).

-include("platform.hrl").

-type(source_type() :: gd | undefined).

-record(meta_doc, {meta_id      :: string(),
		   source_type  :: string(),
		   title        :: string(),
		   modified     :: integer()}). % UNIX time

-record(source_doc, {title      :: string(),
		     modified   :: integer()}). % UNIX time

%%------------------------------------------------------------------------------
%%
%% Run sync process by goven collection ID.
%%
%%------------------------------------------------------------------------------
-spec(sync(Id::integer()) -> term()).

sync(Id) ->
    {SourceType, MetaDocs} = get_meta(Id),
    SourceDocs = get_sources(SourceType, Id),
    lists:foreach(get_fun_delete(SourceDocs), MetaDocs),
    lists:foreach(get_fun_update(SourceType, Id, MetaDocs), SourceDocs).

%%------------------------------------------------------------------------------
%%
%% Get metadata info.
%%
%%------------------------------------------------------------------------------
-spec(get_meta(CollectionId::integer()) -> {source_type(), [#meta_doc{}]}).

get_meta(CollectionId) ->
    Url = ?META_URL ++ "/collections/" ++ integer_to_list(CollectionId),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    {source_type(Fields), get_metadocs(CollectionId)};
	{ok, {{_, 404, _}, _, _}} -> {error, not_found};
	_ -> {error, undefined}
    end.

source_type(Fields) ->
    {struct, Source} = proplists:get_value(<<"source">>, Fields),
    case proplists:get_value(<<"type">>, Source) of
	<<"gd">> -> gd;
	_ -> error
    end.

get_metadocs(CollectionId) ->
    Url = ?META_URL ++ "/collections/" ++ integer_to_list(CollectionId) ++ "/documents/",
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [meta_doc(X) || X <- mochijson2:decode(Body)];
	{ok, {{_, 404, _}, _, _}} -> {error, not_found};
	_ -> {error, undefined}
    end.

meta_doc({struct, Fields}) ->
    {struct, Source} = proplists:get_value(<<"source">>, Fields),
    {proplists:get_value(<<"id">>, Source),
     #meta_doc{meta_id = proplists:get_value(<<"id">>, Fields),
	       source_type = proplists:get_value(<<"type">>, Source),
	       title = proplists:get_value(<<"title">>, Fields),
	       modified = proplists:get_value(<<"modified">>, Fields)}}.

%%------------------------------------------------------------------------------
%%
%% Get list of info about source documents.
%%
%%------------------------------------------------------------------------------
-spec(get_sources(source_type(), CollectionId::integer()) -> [#source_doc{}]).

get_sources(gd, CollectionId) ->
    Url = ?GD_URL ++ "/api/collection/" ++ integer_to_list(CollectionId) ++ "/documents",
    case httpc:request(Url) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [source_doc(X) || X <- mochijson2:decode(Body)];
	{ok, {{_, 404, _}, _, _}} ->
	    [error_get_source];
	_ ->
	    [error_get_source]
    end.

source_doc({struct, Fields}) ->
    {proplists:get_value(<<"id">>, Fields),
     #source_doc{title = proplists:get_value(<<"title">>, Fields),
		 modified = rfc3339:parse_epoch(
			      binary_to_list(
				proplists:get_value(<<"modified">>, Fields)))}}.

%%------------------------------------------------------------------------------
%%
%% Return function wich is removing document entirely from all specified hosts
%% if there is no corresponding source doc now.
%%
%%------------------------------------------------------------------------------
-spec(get_fun_delete(SourceDocs::list()) -> function()).

get_fun_delete(SourceDocs) ->
    fun ({SourceId, MetaDoc}) ->
	    case proplists:get_value(SourceId, SourceDocs) of
		{source_doc, _, _} -> ok;
		undefined ->
		    Id = MetaDoc#meta_doc.meta_id,
		    platform_transmission:delete_document(Id),
		    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id),
		    case httpc:request(delete, {Url, [?META_AUTH]}, [], []) of
			{ok, {{_, 204, _}, _, _}} -> ok;
			{ok, {{_, 404, _}, _, _}} -> {error, not_found};
			_ -> {error, undefined}
		    end
	    end
    end.

%%------------------------------------------------------------------------------
%%
%% Return function which update meta docs if need, for given source document.
%% Check meta for corresponding document by given source.
%% If it does not found, then create new document.
%% If there is a corresponding document, then check `modified` and `title`
%% and update if need.
%%
%%------------------------------------------------------------------------------
-spec(get_fun_update(SourceType::atom(),
		     CollectionId::integer(),
		     MetaDocs::list()) -> function()).

get_fun_update(SourceTypeAtom, CollectionId, MetaDocs) ->
    SourceType = list_to_binary(atom_to_list(SourceTypeAtom)),
    fun (Document) ->
	    case Document of
		error_get_source ->
		    ok;
		{SourceId, SourceDoc} ->
		    case proplists:get_value(SourceId, MetaDocs) of
			undefined ->
			    create_doc(CollectionId, SourceType, SourceId, SourceDoc);
			{meta_doc, MetaId, SourceType, MetaTitle, MetaModified} ->
			    update_doc(MetaId, MetaTitle, MetaModified, SourceDoc)
		    end
	    end
    end.

%%------------------------------------------------------------------------------
%%
%% Create corresponding document in the metadata server for given source
%% document.
%%
%%------------------------------------------------------------------------------
-spec(create_doc(CollectionId::integer(),
		 SourceType::atom(),
		 SourceId::string(),
		 SourceDoc::term()) -> ok).

create_doc(CollectionId, SourceType, SourceId, SourceDoc) ->
    Fields = {struct, [{<<"title">>, SourceDoc#source_doc.title},
		       {<<"source">>, {struct, [{<<"type">>, SourceType},
						{<<"id">>, SourceId}]}},
		       {<<"collection_id">>, CollectionId},
		       {<<"modified">>, SourceDoc#source_doc.modified}]},

    Request = {?META_URL ++ "/documents/",
	       [?META_AUTH],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},

    case httpc:request(post, Request, [], []) of
	{ok, {{_, 201, _}, _, Body}} ->
	    {struct, ResponseFields} = mochijson2:decode(Body),
	    DocId = proplists:get_value(<<"id">>, ResponseFields),
	    io:format("Document [~p] created~n", [DocId]),
	    ok;
	_ -> ok
    end.

%%------------------------------------------------------------------------------
%%
%% Update corresponding meta document if `title` or `modified` is updated.
%%
%%------------------------------------------------------------------------------
-spec(update_doc(MetaId::string(),
		 MetaTitle::string(),
		 MetaModified::integer(),
		 SourceDoc::term()) -> ok).

update_doc(MetaId, MetaTitle, MetaModified, SourceDoc) ->
    if
	% title modified
	MetaTitle =/= SourceDoc#source_doc.title ->

	    Fields = {struct, [{<<"title">>, SourceDoc#source_doc.title},
			       {<<"modified">>, SourceDoc#source_doc.modified}]},

	    Request = {?META_URL ++ "/documents/" ++ integer_to_list(MetaId),
		       [?META_AUTH],
		       "application/json",
		       iolist_to_binary(mochijson2:encode(Fields))},

	    case httpc:request(post, Request, [], []) of % TODO
		{ok, {{_, 204, _}, _, _}} -> io:format("Title at [~p] modified~n", [MetaId]);
		{ok, {{_, _RespStatus, _}, _, _}} -> ok;
		_ -> ok
	    end;

	% content modified
	MetaModified < SourceDoc#source_doc.modified ->

	    Fields = {struct, [{<<"modified">>, SourceDoc#source_doc.modified}]},

	    Request = {?META_URL ++ "/documents/" ++ integer_to_list(MetaId),
		       [?META_AUTH],
		       "application/json",
		       iolist_to_binary(mochijson2:encode(Fields))},

	    case httpc:request(post, Request, [], []) of % TODO
		{ok, {{_, 204, _}, _, _}} -> io:format("Content at [~p] modified~n", [MetaId]);
		{ok, {{_, _RespStatus, _}, _, _}} -> ok;
		_ -> ok
	    end;

	true -> ok
    end.
