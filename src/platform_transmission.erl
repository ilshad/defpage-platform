%%% Process transmission.

-module(platform_transmission).

%% API
-export([]).

%% testing
-export([]).

-include("platform.hrl")

-record(basic_auth, {username     :: string(),
		     password     :: string()}).

-record(secret_auth, {secret      :: string()}).

-type(auth() :: #basic_auth{} | #secret_auth{}).

-record(rest_transmission, {url   :: string(),
			    auth  :: auth()}).

-record(dirty_transmission, {url  :: string(),
			     auth :: string()}.    

-type(transmisson() :: #rest_transmission{} | #dirty_transmission).

%%------------------------------------------------------------------------------
%%
%% Create entry (transmit document to the host fisrt time) by given document's
%% content and transmission metadata.
%%
%%------------------------------------------------------------------------------
-spec(create_entry(Transmission::#transmission{},
		   
