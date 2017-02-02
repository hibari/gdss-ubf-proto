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
%%% File    : ubf_gdss_plugin.erl
%%% Purpose : ubf gdss plugin implementation
%%%----------------------------------------------------------------------

-module(ubf_gdss_plugin).
-behaviour(ubf_plugin_stateless).

-include("ubf_gdss_plugin.hrl").

%% Required callback API for all UBF contract implementations.
-export([info/0, description/0, keepalive/0]).
-export([moduleStart/1, moduleRestart/1]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1, handlerEvent/1]).

-export([filter_bad_terms/1]).

-import(ubf_plugin_handler, [sendEvent/2, install_handler/2]).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./src/ubf_gdss_plugin").

%% @doc ubf string record
-record('#S',
        {value="" :: string()}).

%% @doc ubf string helper
-define(S(X),
        #'#S'{value=X}).

info() ->
    "I am a stateless server".

description() ->
    "An stateless server programmed by UBF".

keepalive() ->
    ok.

%% @doc start module
moduleStart(_Args) ->
    unused.

%% @doc restart module
moduleRestart(Args) ->
    moduleStart(Args).

%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    ack = install_handler(self(), fun handlerEvent/1),
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> void()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc({do, Table, Ops, Flags, Timeout}) ->
    brick_simple:do(Table, Ops, Flags, Timeout);
handlerRpc({add, Table, Key, Val, ExpTime, Flags, Timeout}) ->
    brick_simple:add(Table, Key, Val, ExpTime, Flags, Timeout);
handlerRpc({replace, Table, Key, Val, ExpTime, Flags, Timeout}) ->
    brick_simple:replace(Table, Key, Val, ExpTime, Flags, Timeout);
handlerRpc({set, Table, Key, Val, ExpTime, Flags, Timeout}) ->
    brick_simple:set(Table, Key, Val, ExpTime, Flags, Timeout);
handlerRpc({rename, Table, Key, NewKey, ExpTime, Flags, Timeout}) ->
    brick_simple:rename(Table, Key, NewKey, ExpTime, Flags, Timeout);
handlerRpc({delete, Table, Key, Flags, Timeout}) ->
    brick_simple:delete(Table, Key, Flags, Timeout);
handlerRpc({get, Table, Key, Flags, Timeout}) ->
    brick_simple:get(Table, Key, Flags, Timeout);
handlerRpc({get_many, Table, Key, Num, Flags, Timeout}) ->
    brick_simple:get_many(Table, Key, Num, Flags, Timeout);
handlerRpc({brick_status, Brick, Node, Timeout}) ->
    try
        {ok, Status} = brick_server:status(Brick, Node, Timeout),
        %% DEBUG file:write_file("/tmp/iiii", term_to_binary(filter_bad_terms(lists:keydelete(hash, 1, Status)))),
        {ok, filter_bad_terms(lists:keydelete(hash, 1, Status))}
    catch _X:_Y ->
            noproc
    end;
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.

handlerEvent(Event) ->
    %% @TODO add your own implementation here
    %% Let's fake it and echo the request
    sendEvent(self(), Event),
    fun handlerEvent/1.

filter_bad_terms(L) when is_list(L) ->
    my_map(fun(X) -> filter_bad_terms(X) end, L);
filter_bad_terms(T) when is_tuple(T) ->
    list_to_tuple(filter_bad_terms(tuple_to_list(T)));
filter_bad_terms(X) when is_atom(X); is_integer(X) ->
    X;
filter_bad_terms(X) ->
    %% Examples (not complete list): is_function(X); is_pid(X); is_float(X),...
    list_to_binary(io_lib:format("~w", [X])).

my_map(F, [H|T]) ->
    [F(H)|my_map(F, T)];
my_map(_F, []) ->
    [];
my_map(F, ImproperList) ->
    F(ImproperList).
