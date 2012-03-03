%%% Run actions. This is the Webmachine resource.

-module(platform_process_resource).

-export([init/1, allowed_methods/2, process_post/2]).

-include("platform.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {{trace, "/tmp"}, undefined}.

allowed_methods(Req, State) -> {['POST'], Req, State}.

process_post(Req, State) ->
    {struct, PropList} = mochijson2:decode(wrq:req_body(Req)),
    run(PropList),
    {true, Req, State}.

%%------------------------------------------------------------------------------
%%
%% Run processes.
%%
%%------------------------------------------------------------------------------
-spec(run([tuple()]) -> ok).

run([{<<"sync_collection_source">>, CollectionId} | _]) ->
    spawn(fun() ->
		  platform_source:sync(CollectionId)
	  end).
