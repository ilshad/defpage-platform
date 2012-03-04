%%% Run actions. This is the Webmachine resource.

-module(platform_process_resource).

-export([init/1, allowed_methods/2, process_post/2]).

-include("platform.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {{trace, "/tmp"}, undefined}.

allowed_methods(Req, State) -> {['POST'], Req, State}.

process_post(Req, State) ->
    {struct, [{Action, Param} | _]} = mochijson2:decode(wrq:req_body(Req)),
    run(list_to_atom(binary_to_list(Action)), Param),
    {true, Req, State}.

%%------------------------------------------------------------------------------
%%
%% Run processes.
%%
%%------------------------------------------------------------------------------
-spec(run(Action::atom(), Params::term()) -> ok).

run(sync_collection_source, CollectionId) ->
    spawn(fun() -> platform_source:sync(CollectionId) end);

run(sync_collection_source_batch, []) ->
    run(sync_collection_source_batch, [0, max_collection_id()]);

run(sync_collection_source_batch, [_StartId, _StopId]) ->
    ok.

%%------------------------------------------------------------------------------
%%
%% Helper util: query metadata server for a max collection id
%%
%%------------------------------------------------------------------------------
-spec(max_collection_id() -> integer()).

max_collection_id() ->
    case httpc:request(?META_URL ++ "/collections/?info=max_id") of
	{ok, {{_, 200, _}, _, Body}} ->
	    {struct, Fields} = mochijson2:decode(Body),
	    proplists:get_value(<<"max_id">>, Fields);
	_ -> error
    end.


