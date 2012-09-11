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

-record(rest_transmission_settings, {url :: string(), auth :: auth()}).
-record(dirty_transmission_settings, {url :: string(), auth :: auth()}).
-type(transmission_settings() :: #rest_transmission_settings{} |
				 #dirty_transmission_settings{}).

-record(doc, {id :: integer(),
	      title :: string(),
	      version :: integer()}).

-record(transmission, {hostdoc_id :: string(),
		       version :: integer(),
		       transmission_settings :: transmission_settings()}).

%%------------------------------------------------------------------------------
%%
%% Update document (create / edit / delete) on every host specified
%% in its transmissions.
%%
%%------------------------------------------------------------------------------
-spec(update_document(Id::integer()) -> ok).

update_document(Id) ->
    Doc = doc(Id),
    Data = map(fun (x) -> {Doc, x} end, transmissions(Id)),
    lists:foreach(process_transmission, Data),
    ok.

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
-spec(transmissions(Id::integer()) -> DocumentTransmissions::list()).

transmissions(Id) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id) ++ "/transmissions/",
    case https:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [transmission(X) || X <- mochijson2:decode(Body)];
	_ ->
	    ok
    end.

transmission({struct, Fields}) ->
    #transmission{hostdoc_id = proplists:get_value(<<"hostdoc_id">>, Fields),
		  version = proplists:get_value(<<"version">>, Fields),
		  transmission_settings =
		      transmission_settings(
			list_to_atom(proplists:get_value(<<"type">>, Fields)),
			proplists:get_value(<<"params">>, Fields))}.

transmission_settings(rest, {struct, Fields}) ->
    #rest_transmission_settings{url = proplists:get_value(<<"url">>, Fields),
				auth = auth(proplists:get_value(<<"authentication">>))};

transmission_settings(dirty, {struct, Fields}) ->
    #dirty_transmission_settings{url = proplists:get_value(<<"url">>, Fields),
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

process_transmission(Doc, Transmission)
  when Transmission#transmission.version == 0 ->
    do_create(Doc, Transmission));

process_transmission(Doc, Transmission)
  when Transmission#transmission.version > 0 ->
    if
	Transmission#transmission.version < Doc#doc.version ->
	    do_edit(Doc, Transmission);
	true ->
	    ok
    end.

process_transmission(_, _) ->
    error.

%%------------------------------------------------------------------------------
%%
%% Process "create" transmission.
%%
%%------------------------------------------------------------------------------
-spec(do_create(#doc{}, #transmission{}) -> ok).

do_create(Doc, #transmission #rest_transmission{url=Url, auth=Auth}) ->
    {Title, Abstract, Body} = content(Doc),
    Fields = {struct, [{<<"title">>, Title},
		       {<<"abstract">>,  Abstract},
		       {<<"body">>, Body}]},
    Request = {Url,
	       [auth_header(Auth)],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(Fields))},
    case httpc:request(post, Request, [], []) of
	{ok, {{_, 201, _}, _, Body}} ->
	    {struct, Res} = mochijson2:decode(Body),
	    HostDocId = proplists:get_value(<<"id">>, Res);
	_ ->
	    error
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
