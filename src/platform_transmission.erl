%%% Process transmission.

-module(platform_transmission).

%% API
-export([]).

%% testing
-export([create_entry/2, content/3]).

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

%%------------------------------------------------------------------------------
%%
%% Transmit document to the host fisrt time by given transmission
%% metadata and documents's content. Return doc Id.
%%
%%------------------------------------------------------------------------------
-spec(create_entry(transmission(), #content{}) -> string()).

create_entry(#rest_transmission{url=Url, auth=Auth}, Content) ->
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

%%------------------------------------------------------------------------------
%%
%% Create content record (data structure).
%%
%%------------------------------------------------------------------------------
-spec(content(Title::string(),
	      Abstract::string(),
	      Body::string()) -> #content{}).

content(Title, Abstract, Body) ->
    #content{title = Title,
	     abstract = Abstract,
	     body = binary_to_list(base64:encode(Body))}.
