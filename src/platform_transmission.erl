%%% Process transmission.

-module(platform_transmission).

%% API
-export([]).

%% export
-testing([do_create/2, content/3]).

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

-record(doc, {id :: integer(),
	      title :: string(),
	      version :: integer()}).

%%------------------------------------------------------------------------------
%%
%% Update document (create / edit / delete) on every host specified
%% by its transmissions settings.
%%
%%------------------------------------------------------------------------------
-spec(update_document(Id::integer()) -> ok).

update_document(Id) ->
    lists:foreach(process_transmission, transmissions(doc(Id))).

%%------------------------------------------------------------------------------
%%
%% Ask metadata server for document base attributes.
%%
%%------------------------------------------------------------------------------
-spec((Id::integer()) -> #doc{}).

doc(Id) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    #doc{id = Id,
		 title = proplists:get_value(<<"title">>, Fields),
		 version = proplists:get_value(<<"version">>, Fields)};
	_ ->
	    error
    end.

%%------------------------------------------------------------------------------
%%
%% Ask metadata server for specified document transmissions and their state.
%%
%%------------------------------------------------------------------------------
-spec(transmissions(Id::integer()) -> list()).

transmissions(Doc) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Doc#doc.id) ++ "/transmissions/",
    case https:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [{Doc, transmission(X)} || X <- mochijson2:decode(Body)];
	_ ->
	    ok
    end.

transmission({struct, Fields}) ->
     {proplists:get_value(<<"hostdoc_id">>, Fields),
      proplists:get_value(<<"version">>, Fields),
      transmission_settings(list_to_atom(proplists:get_value(<<"type">>, Fields)),
			    proplists:get_value(<<"id">>, Fields),
			    proplists:get_value(<<"params">>, Fields))}.

transmission_settings(rest, Id, {struct, Fields}) ->
    #rest_transmission_settings{id = Id,
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
-spec(process_transmission(#doc{}, #transmission{}) -> ok).	     

process_transmission({Doc, {_, 0, TransmissionSettings}}) ->
    do_create(Doc, TransmissionSettings));

process_transmission({Doc, {HostDocId, Version, TransmissionSettings}})
  when Version < Doc#doc.version ->
    do_edit(Doc, Transmission);

process_transmission({Doc, {HostDocId, Version, TransmissionSettings}})
  when Version == Doc#doc.version ->
    ok;

process_transmission(_, _) ->
    error.

%%------------------------------------------------------------------------------
%%
%% Process "create" transmission.
%%
%%------------------------------------------------------------------------------
-spec(do_create(Id, Version, TransmissionSettings) -> ok).

do_create(Doc, #rest_transmission_settings{id=TransmissionId, url=Url, auth=Auth}) ->
    {Title, Abstract, Body} = content(Doc#doc.id),
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
	    save_create(Doc#doc.id, Doc#doc.version, TransmissionId, HostDocId);
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
-spec(do_edit(transmission(), #content{}) -> ok).


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
