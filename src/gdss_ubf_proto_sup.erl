%%%----------------------------------------------------------------------
%%% Copyright (c) 2009-2014 Hibari developers.  All rights reserved.
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
%%% File    : gdss_ubf_proto_sup.erl
%%% Purpose : GDSS UBF top-level supervisor
%%%----------------------------------------------------------------------

-module(gdss_ubf_proto_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/1]).

%% supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start_link(_Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%----------------------------------------------------------------------
%%% Callback functions from supervisor
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok,  {SupFlags,  [ChildSpec]}} |
%%          ignore                          |
%%          {error, Reason}
%%----------------------------------------------------------------------
%% @spec(_Args::term()) -> {ok, {supervisor_flags(), child_spec_list()}}
%% @doc The main GDSS UBF supervisor.

init(_Args) ->
    %% seq_trace:set_token(send, true), seq_trace:set_token('receive', true),

    %% Child_spec = [Name, {M, F, A},
    %%               Restart, Shutdown_time, Type, Modules_used]

    Plugins = [ubf_gdss_plugin, ubf_gdss_stub_plugin],

    CEBF = case application:get_env(gdss_ubf_proto, gdss_ebf_tcp_port) of
               {ok, 0} ->
                   [];
               {ok, EBFPort} ->
                   {ok,EBFMaxConn} = application:get_env(gdss_ubf_proto, gdss_ebf_maxconn),
                   {ok,EBFIdleTimer} = application:get_env(gdss_ubf_proto, gdss_ebf_timeout),
                   {ok,EBFPOTerm} = application:get_env(gdss_ubf_proto, gdss_ebf_process_options),
                   EBFProcessOptions = gmt_util:proplists_int_copy([], EBFPOTerm, [fullsweep_after, min_heap_size]),

                   EBFOptions =
                       [{serverhello, "gdss_meta_server"}, {statelessrpc,true},
                        {proto,ebf}, {maxconn,EBFMaxConn},
                        {idletimer,EBFIdleTimer}, {registeredname,gdss_ebf},
                        {tlog_module,undefined}, {process_options,EBFProcessOptions}],
                   EBFServer =
                       {ebf_server, {ubf_server, start_link, [gdss_meta, Plugins, gmt_util:node_localid_port(EBFPort), EBFOptions]},
                        permanent, 2000, worker, [ebf_server]},

                   [EBFServer]
           end,

    CUBF = case application:get_env(gdss_ubf_proto, gdss_ubf_tcp_port) of
               {ok, 0} ->
                   [];
               {ok, UBFPort} ->
                   {ok,UBFMaxConn} = application:get_env(gdss_ubf_proto, gdss_ubf_maxconn),
                   {ok,UBFIdleTimer} = application:get_env(gdss_ubf_proto, gdss_ubf_timeout),
                   {ok,UBFPOTerm} = application:get_env(gdss_ubf_proto, gdss_ubf_process_options),
                   UBFProcessOptions = gmt_util:proplists_int_copy([], UBFPOTerm, [fullsweep_after, min_heap_size]),

                   UBFOptions =
                       [{serverhello, "gdss_meta_server"}, {statelessrpc,true},
                        {proto,ubf}, {maxconn,UBFMaxConn},
                        {idletimer,UBFIdleTimer}, {registeredname,gdss_ubf},
                        {tlog_module,undefined}, {process_options,UBFProcessOptions}],
                   UBFServer =
                       {ubf_server, {ubf_server, start_link, [gdss_meta1, Plugins, gmt_util:node_localid_port(UBFPort), UBFOptions]},
                        permanent, 2000, worker, [ubf_server]},

                   [UBFServer]
           end,

    CJSF = case application:get_env(gdss_ubf_proto, gdss_jsf_tcp_port) of
               {ok, 0} ->
                   [];
               {ok, JSFPort} ->
                   {ok,JSFMaxConn} = application:get_env(gdss_ubf_proto, gdss_jsf_maxconn),
                   {ok,JSFIdleTimer} = application:get_env(gdss_ubf_proto, gdss_jsf_timeout),
                   {ok,JSFPOTerm} = application:get_env(gdss_ubf_proto, gdss_jsf_process_options),
                   JSFProcessOptions = gmt_util:proplists_int_copy([], JSFPOTerm, [fullsweep_after, min_heap_size]),

                   JSFOptions =
                       [{serverhello, "gdss_meta_server"}, {statelessrpc,true},
                        {proto,jsf}, {maxconn,JSFMaxConn},
                        {idletimer,JSFIdleTimer}, {registeredname,gdss_jsf},
                        {tlog_module,undefined}, {process_options,JSFProcessOptions}],
                   JSFServer =
                       {jsf_server, {ubf_server, start_link, [gdss_meta2, Plugins, gmt_util:node_localid_port(JSFPort), JSFOptions]},
                        permanent, 2000, worker, [jsf_server]},

                   [JSFServer]
           end,

    %% NOTE: This server implements native Thrift Binary Format.
    %% There is no need to have UBF over TBF listener except for test
    %% purposes so it has been removed.
    CTBF = case application:get_env(gdss_ubf_proto, gdss_tbf_tcp_port) of
                {ok, 0} ->
                    [];
                {ok, TBFPort} ->
                    {ok,TBFMaxConn} = application:get_env(gdss_ubf_proto, gdss_tbf_maxconn),
                    {ok,TBFIdleTimer} = application:get_env(gdss_ubf_proto, gdss_tbf_timeout),
                    {ok,TBFPOTerm} = application:get_env(gdss_ubf_proto, gdss_tbf_process_options),
                    TBFProcessOptions = gmt_util:proplists_int_copy([], TBFPOTerm, [fullsweep_after, min_heap_size]),

                    TBFOptions =
                        [{serverhello, undefined}, {startplugin, tbf_gdss_plugin}, {statelessrpc,true},
                         {proto,tbf}, {maxconn,TBFMaxConn},
                         {idletimer,TBFIdleTimer}, {registeredname,gdss_tbf},
                         {tlog_module,undefined}, {process_options,TBFProcessOptions}],
                    TBFServer =
                        {tbf_server, {ubf_server, start_link, [gdss_meta3, [tbf_gdss_plugin], gmt_util:node_localid_port(TBFPort), TBFOptions]},
                         permanent, 2000, worker, [tbf_server]},

                    [TBFServer]
            end,

    {ok, {{one_for_one, 2, 60}, CEBF ++ CUBF ++ CJSF ++ CTBF}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
