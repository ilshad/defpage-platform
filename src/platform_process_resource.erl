%%% Run actions. This is the Webmachine resource.

-module(platform_process_resource).

-export([init/1, allowed_methods/2, process_post/2]).

-include("platform.hrl").
-include_lib("webmachine/include/webmachine.hrl").

init([]) -> {{trace, "/tmp"}, undefined}.

allowed_methods(Req, State) -> {['POST'], Req, State}.

process_post(Req, State) ->
    {struct, [{Action, Param} | _]} = mochijson2:decode(wrq:req_body(Req)),
    spawn(fun() -> run(list_to_atom(binary_to_list(Action)), Param) end),
    {true, Req, State}.

%%------------------------------------------------------------------------------
%%
%% Run processes.
%%
%%------------------------------------------------------------------------------
-spec(run(Action::atom(), Params::term()) -> ok).

run(update_meta, []) ->
    run(update_meta, [0, max_collection_id()]);

run(update_meta, [StartId, StopId]) when StartId =< StopId ->
    run(update_meta, StartId),
    run(update_meta, [StartId+1, StopId]);

run(update_meta, [StartId, StopId]) when StartId > StopId ->
    ok;

run(update_meta, CollectionId) ->
    spawn(fun() -> platform_source:sync(CollectionId) end).

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
