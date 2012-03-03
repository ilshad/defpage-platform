%%% Run actions. Thi sis Webmachine resource.

-module(platform_process_resource).

-export([init/1,
	 allowed_methods/2,
	 process_post/2
	 ]).
	 
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
%%
%%
%%------------------------------------------------------------------------------
-spec(run([tuple()]) -> ok).

run([{"sync_collection_source", CollectionId} | _]) ->
    io:format("Processing: ~p~n", [CollectionId]).

