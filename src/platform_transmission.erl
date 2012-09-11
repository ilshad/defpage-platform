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

-record(rest_transmission, {url :: string(), auth :: auth()}).
-record(dirty_transmission, {url :: string(), auth :: auth()}).
-type(transmission() :: #rest_transmission{} | #dirty_transmission{}).

-record(content, {title :: string(),
		  abstract :: string(),
		  body :: string()}).

-record(doc_meta, {id ::x integer(),
		   title :: string(),
		   version :: integer()}).

-record(doc_transmission, {id :: integer(), % transmission id
			   version :: integer(),
			   transmission :: transmission()}).

%%------------------------------------------------------------------------------
%%
%% Update document (create / edit / delete) on every host according to
%% its transmission settings and its state on the server.
%%
%%------------------------------------------------------------------------------
-spec(update_document(Id::integer()) -> ok).

update_document(Id) ->
    Meta = doc_meta(Id),
    Data = map(fun (x) -> {Meta, x} end, doc_transmissions(Id)),
    lists:foreach(process_transmission, Data),
    ok.

%%------------------------------------------------------------------------------
%%
%% Ask metadata server for document base attributes.
%%
%%------------------------------------------------------------------------------
-spec(doc_meta(Id::integer()) -> #document{}).

doc_meta(Id) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id),
    case httpc:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    #doc_meta{id = Id,
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
-spec(doc_transmissions(Id::integer()) -> DocumentTransmissions::list()).

doc_transmissions(Id) ->
    Url = ?META_URL ++ "/documents/" ++ integer_to_list(Id) ++ "/transmissions/",
    case https:request(get, {Url, [?META_AUTH]}, [], []) of
	{ok, {{_, 200, _}, _, Body}} ->
	    [doc_transmission(X) || X <- mochijson2:decode(Body)];
	_ ->
	    ok
    end.

doc_transmission({struct, Fields}) ->
    #doc_transmission{id = proplists:get_value(<<"id">>, Fields),
		      version = proplists:get_value(<<"version">>, Fields),
		      transmission = col_transmission(
				       list_to_atom(
					 proplists:get_value(<<"type">>, Fields)),
				       proplists:get_Value(<<"params">>, Fields))}.

col_transmission(rest, {struct, Fields}) ->
    #rest_transmission{url = proplists:get_value(<<"url">>, Fields),
		       auth = auth(proplists:get_value(<<"authentication">>))};

col_transmission(dirty, {struct, Fields}) ->
    #dirty_transmission{url = proplists:get_value(<<"url">>, Fields),
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

process_transmission(Meta, Transmission) ->
    Version = Transmission#doc_transmission.version,
    if
	Version == 0 -> do_create(Transmission, content(Meta));
	Version > 0 -> do_edit(Transmission, content(Meta));
	Versoin < 0 -> error
    end.

%%------------------------------------------------------------------------------
%%
%% Do transmission the document to the host fisrt time by given
%% transmission metadata and documents's content. Return doc Id.
%%
%%------------------------------------------------------------------------------
-spec(do_create(transmission(), #content{}) -> string()).

do_create(#rest_transmission{url=Url, auth=Auth}, Content) ->
    ContentFields = {struct, [{<<"title">>, Content#content.title},
			      {<<"abstract">>,  Content#content.abstract},
			      {<<"body">>, Content#content.body}]},
    Request = {Url,
	       [auth_header(Auth)],
	       "application/json",
	       iolist_to_binary(mochijson2:encode(ContentFields))},
    case httpc:request(post, Request, [], []) of
	{ok, {{_, 201, _}, _, Body}} ->
	    {struct, ResponseFields} = mochijson2:decode(Body),
	    proplists:get_value(<<"id">>, ResponseFields);
	_ -> error
    end.

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

%-spec(content(Title::string(),
%	      Abstract::string(),
%	      Body::string()) -> #content{}).
%
%content(Title, Abstract, Body) ->
%    #content{title = Title,
%	     abstract = Abstract,
%	     body = binary_to_list(base64:encode(Body))}.
