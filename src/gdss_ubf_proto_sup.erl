%%%----------------------------------------------------------------------
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

    CEBF = case gmt_config_svr:get_config_value_i(gdss_ebf_tcp_port, 7580) of
               {ok, 0} ->
                   [];
               {ok, EBFPort} ->
                   {ok,EBFMaxConn} = gmt_config_svr:get_config_value_i(gdss_ebf_maxconn, 10000),
                   {ok,EBFIdleTimer} = gmt_config_svr:get_config_value_timeoutsec(gdss_ebf_timeout, 60),

                   EBFOptions =
                       [{serverhello, "gdss_meta_server"}, {statelessrpc,true},
                        {proto,ebf}, {maxconn,EBFMaxConn},
                        {idletimer,EBFIdleTimer}, {registeredname, gdss_ebf},
                        {tlog_module, undefined}],
                   EBFServer =
                       {ebf_server, {ubf_server, start_link, [gdss_meta, [ubf_gdss_plugin], gmt_util:node_localid_port(EBFPort), EBFOptions]},
                        permanent, 2000, worker, [ebf_server]},

                   [EBFServer]
           end,

    CUBF = case gmt_config_svr:get_config_value_i(gdss_ubf_tcp_port, 7581) of
               {ok, 0} ->
                   [];
               {ok, UBFPort} ->
                   {ok,UBFMaxConn} = gmt_config_svr:get_config_value_i(gdss_ubf_maxconn, 10000),
                   {ok,UBFIdleTimer} = gmt_config_svr:get_config_value_timeoutsec(gdss_ubf_timeout, 60),

                   UBFOptions =
                       [{serverhello, "gdss_meta_server"}, {statelessrpc,true},
                        {proto,ubf}, {maxconn,UBFMaxConn},
                        {idletimer,UBFIdleTimer}, {registeredname, gdss_ubf},
                        {tlog_module, undefined}],
                   UBFServer =
                       {ubf_server, {ubf_server, start_link, [gdss_meta1, [ubf_gdss_plugin], gmt_util:node_localid_port(UBFPort), UBFOptions]},
                        permanent, 2000, worker, [ubf_server]},

                   [UBFServer]
           end,

    CJSF = case gmt_config_svr:get_config_value_i(gdss_jsf_tcp_port, 7582) of
               {ok, 0} ->
                   [];
               {ok, JSFPort} ->
                   {ok,JSFMaxConn} = gmt_config_svr:get_config_value_i(gdss_jsf_maxconn, 10000),
                   {ok,JSFIdleTimer} = gmt_config_svr:get_config_value_timeoutsec(gdss_jsf_timeout, 60),

                   JSFOptions =
                       [{serverhello, "gdss_meta_server"}, {statelessrpc,true},
                        {proto,jsf}, {maxconn,JSFMaxConn},
                        {idletimer,JSFIdleTimer}, {registeredname, gdss_jsf},
                        {tlog_module, undefined}],
                   JSFServer =
                       {jsf_server, {ubf_server, start_link, [gdss_meta2, [ubf_gdss_plugin], gmt_util:node_localid_port(JSFPort), JSFOptions]},
                        permanent, 2000, worker, [jsf_server]},

                   [JSFServer]
           end,

    {ok, {{one_for_one, 2, 60}, CEBF ++ CUBF ++ CJSF}}.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------
