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
    walk(transmissions(version(Id))).

walk([ H | T ]) ->
    process_transmission(H),
    update_document(T).

%update_document(Id) ->
%    lists:foreach(process_transmission, transmissions(version(Id))).

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
	    {Id, proplists:get_value(<<"version">>, Fields)};
	_ ->
	    error
    end.

%%------------------------------------------------------------------------------
%%
%% Ask metadata server for all transmissions of taken document.
%%
%%------------------------------------------------------------------------------
-spec(transmissions({Id::integer(), Version::integer()}) -> list()).

transmissions(error) -> [];
transmissions({Id, Version}) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id) ++ "/transmissions/",
    case https:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [{Id, Version, transmission_entry(X)} || X <- mochijson2:decode(Body)];
	_ ->
	    []
    end.

transmission_entry({struct, Fields}) ->
     {proplists:get_value(<<"hostdoc_id">>, Fields),
      proplists:get_value(<<"version">>, Fields),
      transmission_settings(list_to_atom(proplists:get_value(<<"type">>, Fields)),
			    proplists:get_value(<<"id">>, Fields),
			    proplists:get_value(<<"params">>, Fields))}.

transmission_settings(rest, TransmissionId, {struct, Fields}) ->
    #rest_transmission_settings{id = TransmissionId,
				url = proplists:get_value(<<"url">>, Fields),
				auth = auth(proplists:get_value(<<"authentication">>))};

transmission_settings(dirty, Id, {struct, Fields}) ->
    #dirty_transmission_settings{id = Id,
				 url = proplists:get_value(<<"url">>, Fields),
				 auth = auth(proplists:get_value(<<"authentication">>))}.

auth({struct, Fields}) ->
    case proplists:get_value(<<"type">>, Fields) of
	"basic" ->
	    #basic_auth{username = proplists:get_value(<<"username">>),
			password = proplists:get_value(<<"password">>)};
	"x-secret" ->
	    #secret_auth{secret = proplists:get_Value(<<"secret">>)}
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
    do_create(Id, Version, TransmissionSettings);

process_transmission({Id, Version, {HostDocId, TransmissionVersion, TransmissionSettings}})
  when TransmissionVersion < Version ->
    do_edit(Id, Version, HostDocId, TransmissionSettings);

process_transmission({_, Version, {_, TransmissionVersion, _}})
  when TransmissionVersion == Version ->
    ok;

process_transmission(_) ->
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
    {Title, Abstract, Body} = content(Id),
    Fields = {struct, [{<<"title">>, Title},
		       {<<"abstract">>, Abstract},
		       {<<"body">>, Body}]},
    Request = {Url,
	       [auth_header(Auth)],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(post, Request, [], []) of
	{ok, {{_, 201, _}, _, Body}} ->
	    {struct, Res} = mochijson2:decode(Body),
	    HostDocId = proplists:get_value(<<"id">>, Res),
	    save_create(Id, Version, TransmissionId, HostDocId);
	_ ->
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
	{ok, {{_, 204, _}, _, _}} -> ok;
	_ -> error
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

do_edit(_, _, _, #rest_transmission_settings{id=_, url=_, auth=_}) ->
    ok.

%%------------------------------------------------------------------------------
%%
%% Create authorization HTTP header.
%%
%%------------------------------------------------------------------------------
-spec(auth_header(Auth::auth()) -> string()).

auth_header(#basic_auth{username=Username, password=Password}) ->
    LoginPass = Username ++ ":" ++ Password,
    {"Authorization", "Basic " ++ binary_to_list(base64:encode(LoginPass))};

auth_header(#secret_auth{secret=Secret}) ->
    {"X-Secret", Secret}.

%%------------------------------------------------------------------------------
%%
%% Get content.
%%
%%------------------------------------------------------------------------------
-spec(content(Id::integer()) -> {Title::integer(),
				 Abstract::integer(),
				 Body::integer()}).

content(_) ->
    {<<"Foo document">>, <<"What is it about?">>, <<"Just nothing...">>}.
