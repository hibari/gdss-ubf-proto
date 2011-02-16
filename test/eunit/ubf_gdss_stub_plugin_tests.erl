%%%-------------------------------------------------------------------
%%% Copyright (c) 2009-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : ubf_gdss_stub_plugin_tests.erl
%%% Purpose : EUnit tests for ubf_gdss_stub_plugin
%%%-------------------------------------------------------------------

-module(ubf_gdss_stub_plugin_tests).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("ubf_gdss_plugin.hrl").


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
     (all_actual_tests_("gdss_stub",ebf_server,erl))(not_used)
     ++ (all_actual_tests_("gdss_stub",ebf_server,tcp))(not_used)
     ++ (all_actual_tests_("gdss_stub",ubf_server,erl))(not_used)
     ++ (all_actual_tests_("gdss_stub",ubf_server,tcp))(not_used)
     %% ++ (all_actual_tests_("gdss_stub",jsf_server,erl))(not_used)
     %% ++ (all_actual_tests_("gdss_stub",jsf_server,tcp))(not_used)
    }.

all_actual_tests_(Service,ServerId,Proto) ->
    fun (_) ->
            [?_test(test_stub(Service,ServerId,Proto))
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
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S(Service),[{get_value_size,1111}]},Timeout),
    Pid;
client_connect(Service,ServerId,erl,Timeout) ->
    Plugins = [ubf_gdss_stub_plugin],
    [Server] = [Child||{Id,Child,_Type,_Module} <- supervisor:which_children(gdss_ubf_proto_sup), Id==ServerId],
    Options = [{serverhello,"gdss_meta_server"},{statelessrpc,true},{plugins,Plugins},{server,Server}],
    {ok,Pid,?S("gdss_meta_server")} = ubf_client:connect(Plugins, Server, Options, Timeout),
    {reply,{ok,ok},none} = ubf_client:rpc(Pid,{startSession,?S(Service),[{get_value_size,1111}]},Timeout),
    Pid.

client_rpc(Pid,Args) ->
    client_rpc(Pid,Args,infinity).

client_rpc(Pid,Args,Timeout) ->
    RPCReply = ubf_client:rpc(Pid,Args,Timeout),
    case RPCReply of
        {reply,Reply,none} ->
            Reply;
        timeout ->
            timeout
    end.

client_stop(Pid) ->
    ubf_client:stop(Pid).


test_stub(Service,ServerId,Proto) ->
    ubf_gdss_eunit_utils:simple_soft_reset(),

    Pid = client_connect(Service,ServerId,Proto),
    ok = client_rpc(Pid,keepalive),

    %% not implemented
    not_implemented = client_rpc(Pid,{get_many, a, <<"">>, 2, [], 1000}),
    not_implemented = client_rpc(Pid, {brick_status, a_ch1_b1, node(), 1000}),

    %% do - ok
    [{ok,0,Binary}] = client_rpc(Pid,{do, a, [{get, <<"foo">>, []}], [], 1000}),

    %% add - ok
    ok = client_rpc(Pid,{add, a, <<"foo">>, <<"bar">>, 0, [], 1000}),
    ok = client_rpc(Pid,{add, a, <<"foo">>, <<"bar">>, 0, [], 1000}),

    %% replace - ok
    ok = client_rpc(Pid,{replace, a, <<"foo">>, <<"bar">>, 0, [], 1000}),
    ok = client_rpc(Pid,{replace, a, <<"foo">>, <<"bar">>, 0, [], 1000}),

    %% set - ok
    ok = client_rpc(Pid,{set, a, <<"foo">>, <<"bar">>, 0, [], 1000}),
    ok = client_rpc(Pid,{set, a, <<"foo">>, <<"bar">>, 0, [], 1000}),

    %% delete - ok
    ok = client_rpc(Pid,{delete, a, <<"foo">>, [], 1000}),
    ok = client_rpc(Pid,{delete, a, <<"foo">>, [], 1000}),

    %% get - ok
    {ok,0,Binary} = client_rpc(Pid,{get, a, <<"foo">>, [], 1000}),
    {ok,0,Binary} = client_rpc(Pid,{get, b, <<"foo">>, [], 1000}),

    %% check value size
    X = 1111,
    X = byte_size(Binary),

    %% done
    ok = client_stop(Pid),
    ok.
