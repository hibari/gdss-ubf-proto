%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2017 Hibari developers.  All rights reserved.
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
%%% File     : ubf_gdss_eunit_utils.erl
%%% Purpose  : UBF GDSS EUnit Utils
%%%----------------------------------------------------------------------

-module(ubf_gdss_eunit_utils).

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
    all_tables(gdss_brick).

all_tables(gdss_brick) ->
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

create_tables(Nodes, BricksPerChain) when is_list(Nodes) ->
    create_tables(Nodes, BricksPerChain, node()).

create_tables(Nodes, BricksPerChain, GDSSAdmin) when is_list(Nodes) ->
    create_tables(Nodes, BricksPerChain, GDSSAdmin, 0, 0).

create_tables(Bricks, BricksPerChain, GDSSAdmin, NumNodesPerBlock, BlockMultFactor)
  when is_list(Bricks),
       is_integer(NumNodesPerBlock), is_integer(BlockMultFactor) ->
    lists:foreach(
      fun({Tab, _Opts, BigDataP}) ->
              VarPrefixP = true,
              VarPrefixSep = '$/',
              VarPrefixNum = 2,

              MakeChainArgs = [Tab, BricksPerChain, Bricks, NumNodesPerBlock, BlockMultFactor],
              Chains = rpc:call(GDSSAdmin, brick_admin, make_chain_description, MakeChainArgs),

              DataProps =
                  [ maketab_bigdata || BigDataP ],
              %% DISABLE ++ [ maketab_do_logging || DiskLoggingP ]
              %% DISABLE ++ [ maketab_do_sync || SyncWritesP ],

              MakeTableArgs = [DataProps, VarPrefixP, VarPrefixSep, VarPrefixNum, Chains],
              BrickOpts = rpc:call(GDSSAdmin, brick_admin, make_common_table_opts, MakeTableArgs),

              AddTableArgs = [Tab, Chains, BrickOpts],
              ok = rpc:call(GDSSAdmin, brick_admin, add_table, AddTableArgs)
      end, all_tables()),
    ok = rpc:call(GDSSAdmin, brick_admin, spam_gh_to_all_nodes, []),
    ok.

simple_internal_setup() ->
    X = brick_eunit_utils:setup_and_bootstrap(),
    create_tables(),
    wait_for_tables(),
    application:start(gdss_ubf_proto),
    X.

simple_internal_teardown() ->
    application:stop(gdss_ubf_proto),
    brick_eunit_utils:teardown(),
    ok.

simple_soft_reset() ->
    _ = [ ok = simple_soft_reset(Tab) || {Tab,_,_} <- all_tables() ],
    {ok, _} = gmt_otp:reload_config(),
    ok.

simple_soft_reset(Tab) ->
    Fun = fun({K,_TS}, _Acc) ->
                  case brick_simple:delete(Tab, K) of
                      ok ->
                          ok;
                      key_not_exist ->
                          %% @TODO Need to understand this race
                          %% condition ?
                          ok
                  end
          end,
    case brick_simple:fold_key_prefix(Tab, <<>>, Fun, ok, [witness]) of
        {ok,ok,_} ->
            ok;
        {error,_Err,_,_} ->
            simple_soft_reset(Tab)
    end.

simple_hard_reset() ->
    case lists:keymember(gdss_ubf_proto, 1, application:which_applications()) of
        true ->
            %% no way to delete and re-create tables
            simple_soft_reset();
        false ->
            simple_internal_teardown(),
            simple_internal_setup()
    end.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

wait_for_tables() ->
    wait_for_tables(node()).

wait_for_tables(GDSSAdmin) ->
    _ = [ ok = gmt_loop:do_while(fun poll_table/1, {GDSSAdmin,not_ready,Tab})
          || {Tab,_,_} <- all_tables() ],
    ok.

poll_table({GDSSAdmin,not_ready,Tab} = T) ->
    TabCh = gmt_util:atom_ify(gmt_util:list_ify(Tab) ++ "_ch1"),
    case rpc:call(GDSSAdmin, brick_sb, get_status, [chain, TabCh]) of
        {ok, healthy} ->
            {false, ok};
        _ ->
            ok = timer:sleep(50),
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
