%%% Process transmission.

-module(platform_transmission).

%% API
-export([]).

%% testing
-export([create_entry/2]).

-include("platform.hrl").

-record(basic_auth, {username :: string(), password :: string()}).
-record(secret_auth, {secret :: string()}).
-type(auth() :: #basic_auth{} | #secret_auth{}).

-record(rest_transmission, {url :: string(), auth :: auth()}).
-record(dirty_transmission, {url :: string(), auth :: auth()}.
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
-spec(create_entry(Transmission::transmission(),
		   Content::#content{}) -> string()).

create_entry(#rest_transmission=Transmission, Content) ->
    ContentFields = {struct, [{<<"title">>, Content#content.title},
			      {<<"abstract">>,  Content#content.abstract},
			      {<<"body">>, Content#content.body}]},
    Request = {Transmission#rest_transmission.url,
	       [auth_header(Transmission#rest_transmission.auth)],
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

auth_header(#basic_auth=Auth) ->
    LoginPass = Auth#basic_auth.username ++ ":" ++ Auth#basic_auth.password,
    {"Authorization", "Basic " ++ binary_to_list(base64:encode(LoginPass))};

auth_header(#secret_auth=Auth) ->
    {"X-Secret", Auth#secret_auth.secret}.
