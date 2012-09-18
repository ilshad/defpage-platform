%%% Process transmission.

-module(platform_transmission).

%% API
-export([]).

%% testing
-export([update_document/1, delete_document/1, delete_entry/2]).

-include("platform.hrl").

-record(basic_auth, {username :: string(), password :: string()}).
-record(secret_auth, {secret :: string()}).
-type(auth() :: #basic_auth{} | #secret_auth{}).

-record(rest_transmission_settings, {id :: integer(),
				     url :: string(),
				     auth :: auth()}).

-record(dirty_transmission_settings, {id :: integer(),
				      url :: string(),
				      auth :: auth()}).

-type(transmission_settings() :: #rest_transmission_settings{} |
				 #dirty_transmission_settings{}).

%%------------------------------------------------------------------------------
%%
%% Update document (create / edit) on every host specified
%% by its transmissions settings.
%%
%%------------------------------------------------------------------------------
-spec(update_document(Id::integer()) -> ok).

update_document(Id) ->
    Version = version(Id),
    All = [{Id, Version, Entry} || Entry <- transmissions(Id)],
    process_transmission(All).

%%------------------------------------------------------------------------------
%%
%% Ask metadata server for document attribute - version.
%%
%%------------------------------------------------------------------------------
-spec(version(DocId::integer()) -> integer()).

version(DocId) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(DocId),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    proplists:get_value(<<"version">>, Fields);
	_ ->
	    error
    end.

%%------------------------------------------------------------------------------
%%
%% Ask metadata server for all transmissions of taken document.
%%
%%------------------------------------------------------------------------------
-spec(transmissions(DocId::integer()) -> list()).

transmissions(DocId) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(DocId) ++ "/transmissions/",
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [transmission_entry(X) || X <- mochijson2:decode(Body)];
	_ ->
	    []
    end.

transmission_entry({struct, Fields}) ->
     {proplists:get_value(<<"hostdoc_id">>, Fields),
      proplists:get_value(<<"version">>, Fields),
      transmission_settings(list_to_atom(binary_to_list(
					   proplists:get_value(<<"type">>, Fields))),
			    proplists:get_value(<<"id">>, Fields),
			    proplists:get_value(<<"params">>, Fields))}.

transmission_settings(rest, TrId, {struct, Fields}) ->
    #rest_transmission_settings{
		       id = TrId,
		       url = binary_to_list(proplists:get_value(<<"url">>, Fields)),
		       auth = auth(proplists:get_value(<<"authentication">>, Fields))};

transmission_settings(dirty, TrId, {struct, Fields}) ->
    #dirty_transmission_settings{
		       id = TrId,
		       url = binary_to_list(proplists:get_value(<<"url">>, Fields)),
		       auth = auth(proplists:get_value(<<"authentication">>, Fields))}.

auth({struct, Fields}) ->
    case proplists:get_value(<<"type">>, Fields) of
	<<"basic">> ->
	    #basic_auth{username = proplists:get_value(<<"username">>, Fields),
			password = proplists:get_value(<<"password">>, Fields)};
	<<"x-secret">> ->
	    #secret_auth{secret = proplists:get_Value(<<"secret">>, Fields)}
    end.

%%------------------------------------------------------------------------------
%%
%% Launch transmission (create / edit) for given document and
%% transmission.
%%
%%------------------------------------------------------------------------------
-spec(process_transmission({DocId::integer(),
			    Version::integer(),
			    {HostDocId::string(),
			     TrVersion::integer(),
			     TrSettings::transmission_settings()
			    }}) -> ok).

process_transmission([ H | T ]) ->
    process_transmission(H),
    process_transmission(T);

process_transmission([]) ->
    ok;

process_transmission({DocId, Version, {_, 0, TrSettings}}) ->
    io:format("Document [~p] is going to be created.~n", [DocId]),
    do_create(DocId, Version, TrSettings);

process_transmission({DocId, Version, {HostDocId, TrVersion, TrSettings}})
  when TrVersion < Version ->
    io:format("Document [~p] is going to be modified from version ~p"
              " to version ~p.~n", [DocId, TrVersion, Version]),
    do_edit(DocId, Version, HostDocId, TrSettings);

process_transmission({DocId, Version, {_, TrVersion, _}})
  when TrVersion == Version ->
    io:format("Document [~p] was tried to modify, but its version (~p)"
              " is actual. So modify is not required yet.~n", [DocId, Version]),
    ok;

process_transmission(_) ->
    io:format("Some ERROR!!!~n"),    
    error.

%%------------------------------------------------------------------------------
%%
%% Process "create" transmission.
%%
%%------------------------------------------------------------------------------
-spec(do_create(DocId::integer(),
		Version::integer(),
		TrSettings::transmission_settings()) -> ok).

do_create(DocId, Version, #rest_transmission_settings{id=TrId, url=Url, auth=Auth}) ->
    {Title, Abstract, Body} = content(create, DocId),
    Fields = {struct, [{<<"title">>, Title},
		       {<<"abstract">>, Abstract},
		       {<<"body">>, Body}]},
    Request = {Url,
	       [auth_header(Auth)],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},

    case httpc:request(post, Request, [], []) of
	{ok, {{_, 201, _}, _, ResponseBody}} ->
	    io:format(":: HTTP POST :: request to the host -> 201 response."
		      " Document id [~p].~n", [DocId]),
	    {struct, Res} = mochijson2:decode(ResponseBody),
	    HostDocId = proplists:get_value(<<"id">>, Res),
	    save_create(DocId, Version, TrId, HostDocId);
	_Res ->
	    error
    end.

%% save info in metadata server
save_create(DocId, Version, TrId, HostDocId) ->
    Fields = {struct, [{<<"transmission_id">>, TrId},
		       {<<"hostdoc_id">>, HostDocId},
		       {<<"created">>, 0},
		       {<<"version">>, Version}]},
    Request = {?META_URL ++ "/documents/" ++ integer_to_list(DocId) ++ "/transmissions/",
	       [?META_AUTH],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(post, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} ->
	    io:format("Document [~p] created and we was remember that."
		      " Host doc id: [~p].~n", [DocId, HostDocId]),
	    ok;
	_Res ->
	    io:format("Some ERROR occured during action of rememebr"
		      " created document [~p].~n", [DocId]),
	    error
    end.

%%------------------------------------------------------------------------------
%%
%% Process "edit" transmission.
%%
%%------------------------------------------------------------------------------
-spec(do_edit(DocId::integer(),
	      Version::integer(),
	      HostDocId::string(),
	      TrSettings::transmission_settings()) -> ok).

do_edit(DocId, Version, HostDocId,
	#rest_transmission_settings{id=TrId, url=Url, auth=Auth}) ->
    {Title, Abstract, Body} = content(edit, DocId),
    Fields = {struct, [{<<"title">>, Title},
		       {<<"abstract">>, Abstract},
		       {<<"body">>, Body}]},
    Request = {Url ++ "/" ++ binary_to_list(HostDocId),
	       [auth_header(Auth)],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(put, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} ->
	    io:format(":: HTTP PUT :: request to the host -> 204 response."
		      " Document id [~p].~n", [DocId]),
	    save_edit(DocId, Version, TrId);
	_Res ->
	    io:format("Transmission ERROR: PUT request to the host."
		      " Document id [~p].~n", [DocId]),
	    error
    end.

%% save info in metadata server
save_edit(DocId, Version, TrId) ->
    Fields = {struct, [{<<"modified">>, 0},
		       {<<"version">>, Version}]},
    Request = {?META_URL ++ "/documents/" ++ integer_to_list(DocId) ++ "/transmissions/"
	       ++ integer_to_list(TrId),
	       [?META_AUTH],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(put, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} ->
	    io:format("Document [~p] modified and we was remember that.~n", [DocId]),
	    ok;
	_Res ->
	    io:format("Some ERROR occured during action of rememebr"
		      " modifed document [~p].~n", [DocId]),
	    error
    end.

%%------------------------------------------------------------------------------
%%
%% Delete document from the host.
%%
%%------------------------------------------------------------------------------
-spec(delete_document(Id::integer()) -> ok).

delete_document([{Id, HostDocId, Settings} | T]) ->
    do_delete(Id, HostDocId, Settings),
    delete_document(T);
delete_document([]) ->
    ok;
delete_document(Id) ->
    delete_document(
      [{Id, HostDocId, Settings} || {HostDocId, _, Settings} <- transmissions(Id)]).

-spec(delete_entry(DocId::integer(), TrId::integer()) -> ok).

delete_entry(DocId, TrId) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(DocId) ++
	"/transmissions/" ++ integer_to_list(TrId),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    HostDocId = proplists:get_value(<<"hostdoc_id">>, Fields),
	    TrSettings = transmission_settings(
			   list_to_atom(binary_to_list(
					  proplists:get_value(<<"type">>, Fields))),
			   TrId,
			   proplists:get_value(<<"params">>, Fields)),
	    do_delete(DocId, HostDocId, TrSettings);
	_Res ->
	    error
    end.

-spec(do_delete(DocId::integer(), HostDocId::string(), transmission_settings()) -> ok).

do_delete(DocId, HostDocId, #rest_transmission_settings{id=TrId, url=Url, auth=Auth}) ->
    Request = {Url ++ "/" ++ binary_to_list(HostDocId), [auth_header(Auth)]},
    case httpc:request(delete, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} ->
	    io:format(":: HTTP DELETE :: request to the host -> 204 response."
		      " Document id [~p].~n", [DocId]),
	    save_delete(DocId, TrId);
	_Res ->
	    io:format("Transmission ERROR: DELETE request for document [~p],"
		      " transmission [~p] has been failed.~n", [DocId, TrId]),
	    error
    end.

save_delete(DocId, TrId) ->
    Request = {?META_URL ++ "/documents/" ++ integer_to_list(DocId) ++ "/transmissions/"
	       ++ integer_to_list(TrId),
	       [?META_AUTH]},
    case httpc:request(delete, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} ->	
	    io:format("Document [~p] with transmission [~p] was dropped"
		      " succefuly.~n", [DocId, TrId]),
	    ok;
	_Res ->
	    io:format("Document [~p] and transmission [~p] was dropped on the"
		      " host but some problems occured when trying rememebr that.~n",
		      [DocId, TrId]),
	    error
    end.

%%------------------------------------------------------------------------------
%%
%% Create authorization HTTP header.
%%
%%------------------------------------------------------------------------------
-spec(auth_header(Auth::auth()) -> string()).

auth_header(#basic_auth{username=Username, password=Password}) ->
    LoginPass = binary_to_list(Username) ++ ":" ++ binary_to_list(Password),
    {"Authorization", "Basic " ++ binary_to_list(base64:encode(LoginPass))};

auth_header(#secret_auth{secret=Secret}) ->
    {"X-Secret", Secret}.

%%------------------------------------------------------------------------------
%%
%% Get content.
%%
%%------------------------------------------------------------------------------
-spec(content(atom(), DocId::integer()) -> {string(), string(), string()}).

content(create, _) ->
    {<<"First created test document">>,
     <<"This is absract from first created document">>,
     base64:encode(<<"This is body od first created document.">>)};

content(edit, _) ->
    {<<"Modified document">>,
     <<"This is modified abstract">>,
     base64:encode(<<"The body of this document was modified!">>)}.
