%%%----------------------------------------------------------------------
%%% Copyright: (c) 2008-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% File     : api_gdss_init.erl
%%% Purpose  : API GDSS
%%%----------------------------------------------------------------------

-module(api_gdss_ubf_proto_init).

-include("ubf_gdss_plugin.hrl").

%% application callbacks
-export([
         %% tables
         all_tables/0
         , all_tables/1
         , create_tables/0
         , create_tables/1
         , create_tables/2
         , create_tables/3
         , create_tables/5
         , wait_for_tables/0
         , wait_for_tables/1
        ]).

-export([
         %% internal
         simple_internal_setup/0
         , simple_internal_teardown/0
         %% reset
         , simple_soft_reset/0
         , simple_hard_reset/0
        ]).

%% Short-term debugging stuff....
-export([go_async/0, go_sync/0, checkpoint/0, running_bricks/0]).

%%%----------------------------------------------------------------------
%%% External API
%%%----------------------------------------------------------------------

all_tables() ->
    all_tables(gdss).

all_tables(gdss) ->
    [
     {a, [], false}
     , {b, [], false}
     , {c, [], false}
     , {a_big, [], true}
     , {b_big, [], true}
     , {c_big, [], true}
    ].

create_tables() ->
    create_tables([node()]).

create_tables(Nodes) when is_list(Nodes) ->
    create_tables(Nodes, 1).

create_tables(Nodes, ChainLen) when is_list(Nodes) ->
    create_tables(Nodes, ChainLen, node()).

create_tables(Nodes, ChainLen, GDSSAdmin) when is_list(Nodes) ->
    create_tables(Nodes, ChainLen, GDSSAdmin, 0, 0).

create_tables(Nodes, ChainLen, GDSSAdmin, NumNodesPerBlock, BlockMultFactor)
  when is_list(Nodes),
       is_integer(NumNodesPerBlock), is_integer(BlockMultFactor) ->
    lists:map(
      fun({Tab, _Opts, BigP}) ->
              ChDesc = rpc:call(GDSSAdmin, brick_admin, make_chain_description,
                                [Tab, ChainLen, Nodes,
                                 NumNodesPerBlock, BlockMultFactor]),
              ChWeights = [{Ch, 100} || {Ch, _} <- ChDesc],
              ok = rpc:call(GDSSAdmin, brick_admin, add_table,
                            [{global,brick_admin},
                             Tab,
                             ChDesc,
                             [{hash_init, fun brick_hash:chash_init/3},
                              {new_chainweights, ChWeights},
                              {prefix_method, var_prefix},
                              {prefix_separator, $/},
                              {num_separators, 2},
                              {implementation_module, brick_ets},
                              if BigP -> {bigdata_dir, "."};
                                 true -> ignored_property
                              end,
                              {do_logging, true},
                              {do_sync, true}]
                            ])
      end, all_tables()),
    ok = rpc:call(GDSSAdmin, brick_admin, spam_gh_to_all_nodes, []),
    ok.


simple_internal_setup() ->
    application:stop(gdss_admin),
    application:stop(gdss_client),
    application:stop(gdss),
    os:cmd("rm -fr Schema.local hlog.*"),
    application:start(gdss),
    application:start(gdss_client),
    application:start(gdss_admin),
    brick_admin:bootstrap_local([], true, $/, 3, 1, 1, []),
    create_tables(),
    wait_for_tables(),
    ok.

simple_internal_teardown() ->
    application:stop(gdss_admin),
    application:stop(gdss_client),
    application:stop(gdss),
    os:cmd("rm -fr Schema.local hlog.*"),
    ok.

simple_soft_reset() ->
    [ ok = simple_soft_reset(Tab) || {Tab,_,_} <- all_tables() ],
    {ok, _} = gmt_otp:reload_config(),
    ok.

simple_soft_reset(Tab) ->
    Fun = fun({K,_TS}, _Acc) -> ok = brick_simple:delete(Tab, K) end,
    case brick_simple:fold_key_prefix(Tab, <<>>, Fun, ok, [witness]) of
        {ok,ok,_} ->
            ok;
        {error,_Err,_,_} ->
            simple_soft_reset(Tab)
    end.

simple_hard_reset() ->
    %% no way to delete and re-create tables
    simple_soft_reset().

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

wait_for_tables() ->
    wait_for_tables(node()).

wait_for_tables(GDSSAdmin) ->
    [ gmt_loop:do_while(fun poll_table/1, {GDSSAdmin,not_ready,Tab})
      || {Tab,_,_} <- all_tables() ].

poll_table({GDSSAdmin,not_ready,Tab} = T) ->
    TabCh = gmt_util:atom_ify(gmt_util:list_ify(Tab) ++ "_ch1"),
    case rpc:call(GDSSAdmin, brick_sb, get_status, [chain, TabCh]) of
        {ok, healthy} ->
            {false, ok};
        _ ->
            ok = timer:sleep(250),
            {true, T}
    end.

%%%----------------------------------------------------------------------
%%% Debug functions
%%%----------------------------------------------------------------------

go_sync() ->
    go_sync(true).

go_async() ->
    go_sync(false).

go_sync(Bool) ->
    [brick_server:set_do_sync(Br, Bool) || {_Tab, Brs} <- running_bricks(),
                                           Br <- Brs].

checkpoint() ->
    [brick_server:checkpoint(Br, Nd) || {_Tab, Brs} <- running_bricks(),
                                        {Br, Nd} <- Brs].

running_bricks() ->
    lists:flatten(lists:map(
                    fun(Tab) ->
                            {ok, Ps} = brick_admin:get_table_info(
                                         {global, brick_admin}, Tab),
                            GH = proplists:get_value(ghash, Ps),
                            Cs = lists:usort(brick_hash:all_chains(GH, current)
                                             ++
                                             brick_hash:all_chains(GH, new)),
                            {Tab, [Br || {_Ch, Brs} <- Cs, Br <- Brs]}
                    end, [tab1] ++ [Tab || {Tab, _, _} <- all_tables()])).
