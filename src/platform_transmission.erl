%%% Process transmission.

-module(platform_transmission).

%% API
-export([]).

%% testing
-export([update_document/1]).

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
%% Update document (create / edit / delete) on every host specified
%% by its transmissions settings.
%%
%%------------------------------------------------------------------------------
-spec(update_document(Id::integer()) -> ok).

update_document(Id) ->
    walk(transmissions(Id, version(Id))).

walk([ H | T ]) ->
    process_transmission(H),
    walk(T);

walk([]) ->
    ok.

%%------------------------------------------------------------------------------
%%
%% Ask metadata server for document attribute - version.
%%
%%------------------------------------------------------------------------------
-spec(version(Id::integer()) -> {Id::integer(), Version::integer()}).

version(Id) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id),
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
-spec(transmissions(Id::integer(), Version::integer() | error) -> list()).

transmissions(_, error) -> [];
transmissions(Id, Version) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id) ++ "/transmissions/",
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [{Id, Version, transmission_entry(X)} || X <- mochijson2:decode(Body)];
	_ ->
	    []
    end.

transmission_entry({struct, Fields}) ->
     {proplists:get_value(<<"hostdoc_id">>, Fields),
      proplists:get_value(<<"version">>, Fields),
      transmission_settings(list_to_atom(binary_to_list(proplists:get_value(<<"type">>, Fields))),
			    proplists:get_value(<<"id">>, Fields),
			    proplists:get_value(<<"params">>, Fields))}.

transmission_settings(rest, TransmissionId, {struct, Fields}) ->
    #rest_transmission_settings{id = TransmissionId,
				url = binary_to_list(proplists:get_value(<<"url">>, Fields)),
				auth = auth(proplists:get_value(<<"authentication">>, Fields))};

transmission_settings(dirty, Id, {struct, Fields}) ->
    #dirty_transmission_settings{id = Id,
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
%% Launch transmission (create / edit / delete) for given document and
%% transmission.
%%
%%------------------------------------------------------------------------------
-spec(process_transmission({Id::integer(), % document id
			    Version::integer(), % actual document version
			    {HostDocId::string(), % document id form the host
			     TransmissionVersion::integer(), % transmitted version
			     TransmissionSettings::transmission_settings()
			    }}) -> ok).

process_transmission({Id, Version, {_, 0, TransmissionSettings}}) ->
    io:format("Document [~p] is going to be created.~n", [Id]),
    do_create(Id, Version, TransmissionSettings);

process_transmission({Id, Version, {HostDocId, TransmissionVersion, TransmissionSettings}})
  when TransmissionVersion < Version ->
    io:format("Document [~p] is going to be modified from version ~p to version ~p.~n", [Id, TransmissionVersion, Version]),
    do_edit(Id, Version, HostDocId, TransmissionSettings);

process_transmission({_, Version, {_, TransmissionVersion, _}})
  when TransmissionVersion == Version ->
    io:format("Document [~p] was tried to modify, but its version (~p) is actual. So modify is not required yet.", [Id. Version]),
    ok;

process_transmission(_) ->
    io:format("Some ERROR!!!"),    
    error.

%%------------------------------------------------------------------------------
%%
%% Process "create" transmission.
%%
%%------------------------------------------------------------------------------
-spec(do_create(Id::integer(),
		Version::integer(),
		TransmissionSettings::transmission_settings()) -> ok).

do_create(Id, Version, #rest_transmission_settings{id=TransmissionId, url=Url, auth=Auth}) ->
    {Title, Abstract, Body} = content(create, Id),
    Fields = {struct, [{<<"title">>, Title},
		       {<<"abstract">>, Abstract},
		       {<<"body">>, Body}]},
    Request = {Url,
	       [auth_header(Auth)],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},

    case httpc:request(post, Request, [], []) of
	{ok, {{_, 201, _}, _, ResponseBody}} ->
	    {struct, Res} = mochijson2:decode(ResponseBody),
	    HostDocId = proplists:get_value(<<"id">>, Res),
	    save_create(Id, Version, TransmissionId, HostDocId);
	_Res ->
	    error
    end.

%% save info in metadata server
save_create(DocId, Version, TransmissionId, HostDocId) ->
    Fields = {struct, [{<<"transmission_id">>, TransmissionId},
		       {<<"hostdoc_id">>, HostDocId},
		       {<<"created">>, 0},
		       {<<"version">>, Version}]},
    Request = {?META_URL ++ "/documents/" ++ integer_to_list(DocId) ++ "/transmissions/",
	       [?META_AUTH],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(post, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} ->
	    io:format("Document [~p] created and we was remember that. Host doc id: [~s].~n", [DocId, HostDocId]),
	    ok;
	_Res ->
	    io:format("Some ERROR occured during action of rememebr created document [~p].~n", [DocId]),
	    error
    end.

%%------------------------------------------------------------------------------
%%
%% Process "edit" transmission.
%%
%%------------------------------------------------------------------------------
-spec(do_edit(Id::integer(),
	      Version::integer(),
	      HostDocId::string(),
	      TransmissionSettings::transmission_settings()) -> ok).

do_edit(Id, Version, HostDocId,
	#rest_transmission_settings{id=TransmissionId, url=Url, auth=Auth}) ->
    {Title, Abstract, Body} = content(edit, Id),
    Fields = {struct, [{<<"title">>, Title},
		       {<<"abstract">>, Abstract},
		       {<<"body">>, Body}]},
    Request = {Url ++ "/" ++ HostDocId,
	       [auth_header(Auth)],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(put, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} -> save_edit(Id, Version, TransmissionId);
	_Res -> error
    end.

%% save info in metadata server
save_edit(DocId, Version, TransmissionId) ->
    Fields = {struct, [{<<"modified">>, 0},
		       {<<"version">>, Version}]},
    Request = {?META_URL ++ "/documents/" ++ integer_to_list(DocId) ++ "/transmissions/"
	       ++ integer_to_list(TransmissionId),
	       [?META_AUTH],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(post, Request, [], []) of
	{ok, {{_, 204, _}, _, _}} ->
	    io:format("Document [~p] modified and we was remember that.~n", [DocId]),
	    ok;
	_Res ->
	    io:format("Some ERROR occured during action of rememebr modifed document [~p].~n", [DocId]),
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
-spec(content(atom(), Id::integer()) -> {Title::string(),
				 Abstract::string(),
				 Body::string()}).

content(create, _) ->
    {<<"First created test document">>,
     <<"This is absract from first created document">>,
     base64:encode(<<"This is body od first created document.">>)};

content(edit, _) ->
    {<<"Modified document">>,
     <<"This is modified abstract">>,
     base64:encode(<<"The body of this document was modified!">>)}.
