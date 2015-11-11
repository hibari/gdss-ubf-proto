%%%-------------------------------------------------------------------
%%% Copyright (c) 2009-2015 Hibari developers.  All rights reserved.
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
%%% File    : ubf_gdss_plugin_tests.erl
%%% Purpose : EUnit tests for ubf_gdss_plugin
%%%-------------------------------------------------------------------

-module(ubf_gdss_plugin_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("ubf_gdss_plugin.hrl").

%% @doc ubf string record
-record('#S',
        {value="" :: string()}).

%% @doc ubf string helper
-define(S(X),
        #'#S'{value=X}).

-define(TIMEOUT, 1000).

%%%----------------------------------------------------------------------
%%% TESTS
%%%----------------------------------------------------------------------

all_tests_test_() ->
    all_tests_(fun test_setup/0,
               fun test_teardown/1).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

all_tests_(Setup,Teardown) ->
    {setup,
     Setup,
     Teardown,
     (all_actual_tests_("gdss",ebf_server,erl))(not_used)
     ++ (all_actual_tests_("gdss",ebf_server,tcp))(not_used)
     ++ (all_actual_tests_("gdss",ubf_server,erl))(not_used)
     ++ (all_actual_tests_("gdss",ubf_server,tcp))(not_used)
     %% ++ (all_actual_tests_("gdss",jsf_server,erl))(not_used)
     %% ++ (all_actual_tests_("gdss",jsf_server,tcp))(not_used)
    }.

all_actual_tests_(Service,ServerId,Proto) ->
    fun (_) ->
            [?_test(test_get_add_set(Service, ServerId, Proto)),
             ?_test(test_simplified(Service, ServerId, Proto)),
             ?_test(test_simple_replace1(Service, ServerId, Proto)),
             ?_test(test_simple_rename1(Service, ServerId, Proto)),
             ?_test(test_status(Service, ServerId, Proto))
            ]
    end.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

test_setup() ->
    ubf_gdss_eunit_utils:simple_internal_setup(),
    ubf_gdss_eunit_utils:simple_hard_reset(),
    ok.

test_teardown(_) ->
    ubf_gdss_eunit_utils:simple_internal_teardown(),
    ok.

client_connect(Service,ServerId,Proto) ->
    client_connect(Service,ServerId,Proto,infinity).

client_connect(Service,ServerId,tcp,Timeout) ->
    {Port,Proto} =
        if ServerId =:= ebf_server -> {7580, ebf};
           ServerId =:= ubf_server -> {7581, ubf};
           ServerId =:= jsf_server -> {7582, jsf}
        end,
    {ok,Pid,?S("gdss_meta_server")} = ubf_client:connect("localhost",Port,[{proto,Proto}],Timeout),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S(Service),[]},Timeout),
    Pid;
client_connect(Service,ServerId,erl,Timeout) ->
    Plugins = [ubf_gdss_plugin],
    [Server] = [Child||{Id,Child,_Type,_Module} <- supervisor:which_children(gdss_ubf_proto_sup), Id==ServerId],
    Options = [{serverhello,"gdss_meta_server"},{statelessrpc,true},{plugins,Plugins},{server,Server}],
    {ok,Pid,?S("gdss_meta_server")} = ubf_client:connect(Plugins, Server, Options, Timeout),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S(Service),[]},Timeout),
    Pid.

client_rpc(Pid,Args) ->
    client_rpc(Pid, Args, infinity).

client_rpc(Pid, Args, Timeout) ->
    RPCReply = ubf_client:rpc(Pid, Args, Timeout),
    case RPCReply of
        {reply, Reply, none} ->
            Reply;
        timeout ->
            timeout
    end.

client_stop(Pid) ->
    ubf_client:stop(Pid).


test_get_add_set(Service, ServerId, Proto) ->
    ubf_gdss_eunit_utils:simple_soft_reset(),

    Pid = client_connect(Service,ServerId,Proto),
    ok = client_rpc(Pid,keepalive),

    %% get - ng
    [key_not_exist] = client_rpc(Pid,{do, a, [{get, <<"foo">>, []}], [], ?TIMEOUT}),

    %% add - ok
    [{ok, 1}] = client_rpc(Pid,{do, a, [{add, <<"foo">>, 1, <<"bar">>, 0, []}], [], ?TIMEOUT}),
    %% add - ng
    [{key_exists,1}] = client_rpc(Pid,{do, a, [{add, <<"foo">>, 1, <<"bar">>, 0, []}], [], ?TIMEOUT}),
    %% get - ok
    [{ok,1,<<"bar">>}] = client_rpc(Pid,{do, a, [{get, <<"foo">>, []}], [], ?TIMEOUT}),

    %% set - ok
    [{ok, 2}] = client_rpc(Pid,{do, a, [{set, <<"foo">>, 2, <<"baz">>, 0, []}], [], ?TIMEOUT}),
    %% get - ok
    [{ok, 2, <<"baz">>}] = client_rpc(Pid,{do, a, [{get, <<"foo">>, []}], [], ?TIMEOUT}),

    ok = client_stop(Pid),
    ok.

test_simplified(Service, ServerId, Proto) ->
    ubf_gdss_eunit_utils:simple_soft_reset(),

    Pid = client_connect(Service,ServerId,Proto),
    ok = client_rpc(Pid,keepalive),

    %% get - ng
    key_not_exist = client_rpc(Pid,{get, a, <<"foo">>, [], ?TIMEOUT}),

    %% add - ok
    {ok, _} = client_rpc(Pid,{add, a, <<"foo">>, <<"bar">>, 0, [], ?TIMEOUT}),
    %% add - ng
    {key_exists, _} = client_rpc(Pid,{add, a, <<"foo">>, <<"bar">>, 0, [], ?TIMEOUT}),
    %% get - ok
    {ok, _, <<"bar">>} = client_rpc(Pid,{get, a, <<"foo">>, [], ?TIMEOUT}),

    %% set - ok
    {ok, _} = client_rpc(Pid,{set, a, <<"foo">>, <<"baz">>, 0, [], ?TIMEOUT}),
    %% get - ok
    {ok, _, <<"baz">>} = client_rpc(Pid,{get, a, <<"foo">>, [], ?TIMEOUT}),

    {ok, _} = client_rpc(Pid,{replace, a, <<"foo">>, <<"baz">>, 0, [], ?TIMEOUT}),
    key_not_exist = client_rpc(Pid,{replace, a, <<"foo-not-exist">>, <<"baz">>, 0, [], ?TIMEOUT}),

    {ok, _} = client_rpc(Pid,{set, a, <<"foo">>, <<"baz">>, 0, [], ?TIMEOUT}),
    {ok, _} = client_rpc(Pid,{set, a, <<"bar">>, <<"baz">>, 0, [], ?TIMEOUT}),
    {ok, _} = client_rpc(Pid,{set, a, <<"baz">>, <<"baz">>, 0, [], ?TIMEOUT}),
    {ok, {[_,_], true}} = client_rpc(Pid,{get_many, a, <<"">>, 2, [], ?TIMEOUT}),
    {ok, {[_,_,_], false}} = client_rpc(Pid,{get_many, a, <<"">>, 3, [], ?TIMEOUT}),
    {ok, {[], false}} = client_rpc(Pid,{get_many, a, <<"zzzz">>, 99, [], ?TIMEOUT}),

    ok = client_rpc(Pid,{delete, a, <<"foo">>, [], ?TIMEOUT}),
    key_not_exist = client_rpc(Pid,{delete, a, <<"foo">>, [], ?TIMEOUT}),
    ok = client_rpc(Pid,{delete, a, <<"bar">>, [], ?TIMEOUT}),
    ok = client_rpc(Pid,{delete, a, <<"baz">>, [], ?TIMEOUT}),

    ok = client_stop(Pid),
    ok.

%% @doc test exp_time_directive and attrib_directive in replace operation
test_simple_replace1(Service, ServerId, Proto) ->
    ubf_gdss_eunit_utils:simple_soft_reset(),

    Pid = client_connect(Service, ServerId, Proto),
    ok = client_rpc(Pid, keepalive),

    Table  = a,

    KeyPrefix = <<"/100/1">>,
    Key    = <<"/100/1/A">>,

    ValA   = <<"AAA">>,
    ExpA   = make_exp(5000 * 1000),
    FlagsA = [{color, blue}],

    ValB   = <<"BBB">>,
    ExpB   = make_exp(360 * 1000),
    FlagsB = [{shape, triangle}],

    ValC   = <<"CCC">>,
    FlagsC = [{color, green}, {exp_time_directive, keep}, {attrib_directive, keep}],

    %% reset
    _ = client_rpc(Pid, {delete, Table, Key, [], ?TIMEOUT}),

    %% add KeyA
    {ok, _} = client_rpc(Pid, {add, Table, Key, ValA, ExpA, FlagsA, ?TIMEOUT}),

    %% get_many
    {ok, {[{Key, _TS1, ValA, ExpA, Flags1}], false}} =
        client_rpc(Pid, {get_many, Table, KeyPrefix, 100, [get_all_attribs], ?TIMEOUT}),
    blue = proplists:get_value(color, Flags1),

    %% replace KeyA
    {ok, _} = client_rpc(Pid, {replace, Table, Key, ValB, ExpB, FlagsB, ?TIMEOUT}),
    {ok, {[{Key, _TS2, ValB, ExpB, Flags2}], false}} =
        client_rpc(Pid, {get_many, Table, KeyPrefix, 100, [get_all_attribs], ?TIMEOUT}),
    undefined = proplists:get_value(color, Flags2),
    triangle = proplists:get_value(shape, Flags2),

    %% replace KeyA again
    {ok, _} = client_rpc(Pid, {replace, Table, Key, ValC, 0, FlagsC, ?TIMEOUT}),
    {ok, {[{Key, _TS3, ValC, ExpB, Flags3}], false}} =
        client_rpc(Pid, {get_many, Table, KeyPrefix, 100, [get_all_attribs], ?TIMEOUT}),
    green = proplists:get_value(color, Flags3),
    triangle = proplists:get_value(shape, Flags3).

%% @doc test exp_time_directive and attrib_directive in rename operation
test_simple_rename1(Service, ServerId, Proto) ->
    ubf_gdss_eunit_utils:simple_soft_reset(),

    Pid = client_connect(Service, ServerId, Proto),
    ok = client_rpc(Pid, keepalive),

    Table  = a,
    KeyPrefix = <<"/100/1">>,

    KeyA   = <<"/100/1/A">>,
    Val    = <<"AAA">>,
    ExpA   = make_exp(5000 * 1000),
    FlagsA = [{color, blue}],

    KeyB   = <<"/100/1/B">>,
    ExpB   = make_exp(360 * 1000),
    FlagsB = [{shape, triangle}, {exp_time_directive, replace}, {attrib_directive, replace}],

    KeyC   = <<"/100/1/C">>,
    FlagsC = [{color, green}],

    %% reset
    _ = client_rpc(Pid, {delete, Table, KeyA, [], ?TIMEOUT}),
    _ = client_rpc(Pid, {delete, Table, KeyB, [], ?TIMEOUT}),
    _ = client_rpc(Pid, {delete, Table, KeyC, [], ?TIMEOUT}),

    %% add KeyA
    {ok, _} = client_rpc(Pid, {add, Table, KeyA, Val, ExpA, FlagsA, ?TIMEOUT}),

    %% get_many
    {ok, {[{KeyA, _TS1, Val, ExpA, Flags1}], false}} =
        client_rpc(Pid, {get_many, Table, KeyPrefix, 100, [get_all_attribs], ?TIMEOUT}),
    blue = proplists:get_value(color, Flags1),

    %% rename KeyA to KeyB
    {ok, _} = client_rpc(Pid, {rename, Table, KeyA, KeyB, ExpB, FlagsB, ?TIMEOUT}),
    {ok, {[{KeyB, _TS2, Val, ExpB, Flags2}], false}} =
        client_rpc(Pid, {get_many, Table, KeyPrefix, 100, [get_all_attribs], ?TIMEOUT}),
    undefined = proplists:get_value(color, Flags2),
    triangle = proplists:get_value(shape, Flags2),

    %% rename KeyB to KeyC
    {ok, _} = client_rpc(Pid, {rename, Table, KeyB, KeyC, 0, FlagsC, ?TIMEOUT}),
    {ok, {[{KeyC, _TS3, Val, ExpB, Flags3}], false}} =
        client_rpc(Pid, {get_many, Table, KeyPrefix, 100, [get_all_attribs], ?TIMEOUT}),
    green = proplists:get_value(color, Flags3),
    triangle = proplists:get_value(shape, Flags3).

test_status(Service, ServerId, Proto) ->
    ubf_gdss_eunit_utils:simple_soft_reset(),

    Pid = client_connect(Service,ServerId,Proto),
    ok = client_rpc(Pid,keepalive),

    {ok, _} = client_rpc(Pid, {brick_status, a_ch1_b1, node(), ?TIMEOUT}),
    noproc = client_rpc(Pid, {brick_status, does_not_exist, node(), ?TIMEOUT}),

    ok = client_stop(Pid),
    ok.

-spec make_exp(non_neg_integer()) -> exp_time().
make_exp(StepMillis) ->
    NowX = gmt_time_otp18:system_time(micro_seconds),
    %% TODO: FIXME: This should read (NowX div 1000).
    %% https://github.com/hibari/gdss-admin/issues/11
    (NowX * 1000) + StepMillis.
