%%%-------------------------------------------------------------------
%%% Copyright: (c) 2009-2010 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : ubf_gdss_plugin_test.erl
%%% Purpose : EUnit tests for ubf_gdss_plugin
%%%-------------------------------------------------------------------

-module(ubf_gdss_plugin_test).

-compile(export_all).
-include_lib("eunit/include/eunit.hrl").
-include("ubf_gdss_plugin.hrl").

do_eunit() ->
    case eunit:test(?MODULE) of
        ok -> ok;
        _ -> erlang:halt(1)
    end.

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
            [?_test(test_get_add_set(Service,ServerId,Proto))
             , ?_test(test_simplified(Service,ServerId,Proto))
             , ?_test(test_status(Service,ServerId,Proto))
            ]
    end.

%%%----------------------------------------------------------------------
%%% Internal
%%%----------------------------------------------------------------------

-define(APPS, [gdss_ubf_proto, gdss_admin, gdss_client, gdss, gmt, inets, crypto, sasl]).

test_setup() ->
    %% @TODO - boilerplate start
    os:cmd("rm -rf Schema.local hlog.* root"),
    os:cmd("ln -s ../../gdss-admin/priv/root ."),
    os:cmd("epmd -kill; sleep 1"),
    os:cmd("epmd -daemon; sleep 1"),
    {ok, _} = net_kernel:start(['eunit@localhost', shortnames]),
    [ application:stop(A) || A <- ?APPS ],
    [ ok=application:start(A) || A <- lists:reverse(?APPS) ],
    random:seed(erlang:now()),
    ok = application:set_env(gdss, brick_max_log_size_mb, 1),
    %% @TODO - boilerplate stop
    api_gdss_ubf_proto_init:simple_internal_setup(),
    api_gdss_ubf_proto_init:simple_hard_reset().

test_teardown(_X) ->
    api_gdss_ubf_proto_init:simple_internal_teardown(),
    %% @TODO - boilerplate start
    [ application:stop(A) || A <- ?APPS ],
    ok = net_kernel:stop(),
    %% @TODO - boilerplate stop
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


test_get_add_set(Service,ServerId,Proto) ->
    api_gdss_ubf_proto_init:simple_soft_reset(),

    Pid = client_connect(Service,ServerId,Proto),
    ok = client_rpc(Pid,keepalive),

    %% get - ng
    [key_not_exist] = client_rpc(Pid,{do, a, [{get, <<"foo">>, []}], [], 1000}),

    %% add - ok
    [ok] = client_rpc(Pid,{do, a, [{add, <<"foo">>, 1, <<"bar">>, 0, []}], [], 1000}),
    %% add - ng
    [{key_exists,1}] = client_rpc(Pid,{do, a, [{add, <<"foo">>, 1, <<"bar">>, 0, []}], [], 1000}),
    %% get - ok
    [{ok,1,<<"bar">>}] = client_rpc(Pid,{do, a, [{get, <<"foo">>, []}], [], 1000}),

    %% set - ok
    [ok] = client_rpc(Pid,{do, a, [{set, <<"foo">>, 2, <<"baz">>, 0, []}], [], 1000}),
    %% get - ok
    [{ok,2,<<"baz">>}] = client_rpc(Pid,{do, a, [{get, <<"foo">>, []}], [], 1000}),

    ok = client_stop(Pid),
    ok.

test_simplified(Service,ServerId,Proto) ->
    api_gdss_ubf_proto_init:simple_soft_reset(),

    Pid = client_connect(Service,ServerId,Proto),
    ok = client_rpc(Pid,keepalive),

    %% get - ng
    key_not_exist = client_rpc(Pid,{get, a, <<"foo">>, [], 1000}),

    %% add - ok
    ok = client_rpc(Pid,{add, a, <<"foo">>, <<"bar">>, 0, [], 1000}),
    %% add - ng
    {key_exists,_} = client_rpc(Pid,{add, a, <<"foo">>, <<"bar">>, 0, [], 1000}),
    %% get - ok
    {ok,_,<<"bar">>} = client_rpc(Pid,{get, a, <<"foo">>, [], 1000}),

    %% set - ok
    ok = client_rpc(Pid,{set, a, <<"foo">>, <<"baz">>, 0, [], 1000}),
    %% get - ok
    {ok,_,<<"baz">>} = client_rpc(Pid,{get, a, <<"foo">>, [], 1000}),

    ok = client_rpc(Pid,{replace, a, <<"foo">>, <<"baz">>, 0, [], 1000}),
    key_not_exist = client_rpc(Pid,{replace, a, <<"foo-not-exist">>, <<"baz">>, 0, [], 1000}),

    ok = client_rpc(Pid,{set, a, <<"foo">>, <<"baz">>, 0, [], 1000}),
    ok = client_rpc(Pid,{set, a, <<"bar">>, <<"baz">>, 0, [], 1000}),
    ok = client_rpc(Pid,{set, a, <<"baz">>, <<"baz">>, 0, [], 1000}),
    {ok, {[_,_], true}} = client_rpc(Pid,{get_many, a, <<"">>, 2, [], 1000}),
    {ok, {[_,_,_], false}} = client_rpc(Pid,{get_many, a, <<"">>, 3, [], 1000}),
    {ok, {[], false}} = client_rpc(Pid,{get_many, a, <<"zzzz">>, 99, [], 1000}),

    ok = client_rpc(Pid,{delete, a, <<"foo">>, [], 1000}),
    key_not_exist = client_rpc(Pid,{delete, a, <<"foo">>, [], 1000}),
    ok = client_rpc(Pid,{delete, a, <<"bar">>, [], 1000}),
    ok = client_rpc(Pid,{delete, a, <<"baz">>, [], 1000}),

    ok = client_stop(Pid),
    ok.

test_status(Service,ServerId,Proto) ->
    api_gdss_ubf_proto_init:simple_soft_reset(),

    Pid = client_connect(Service,ServerId,Proto),
    ok = client_rpc(Pid,keepalive),

    {ok, _} = client_rpc(Pid, {brick_status, a_ch1_b1, node(), 1000}),
    noproc = client_rpc(Pid, {brick_status, does_not_exist, node(), 1000}),

    ok = client_stop(Pid),
    ok.
