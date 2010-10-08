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
%%% File    : ubf_gdss_stub_plugin.erl
%%% Purpose : ubf gdss stub plugin implementation
%%%----------------------------------------------------------------------

-module(ubf_gdss_stub_plugin).

-include("ubf.hrl").

-export([info/0, description/0, keepalive/0]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1]).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("ubf_gdss_stub_plugin").
-add_types(ubf_gdss_plugin).

info() ->
    "I am a stateless server".

description() ->
    "An stateless server programmed by UBF".

keepalive() ->
    ok.


%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(Args) ->
    Size = proplists:get_value(get_value_size, Args),
    ok = put_cached_get_value_size(Size),
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> void()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc({do, _Table, [{add, _Key, _TStamp, _Val, _ExpTime, _EFlags}], _Flags, _Timeout}) ->
    [ok];
handlerRpc({do, _Table, [{replace, _Key, _TStamp, _Val, _ExpTime, _EFlags}], _Flags, _Timeout}) ->
    [ok];
handlerRpc({do, _Table, [{set, _Key, _TStamp, _Val, _ExpTime, _EFlags}], _Flags, _Timeout}) ->
    [ok];
handlerRpc({do, _Table, [{delete, _Key, _EFlags}], _Flags, _Timeout}) ->
    [ok];
handlerRpc({do, _Table, [{get, _Key, _EFlags}], _Flags, _Timeout}) ->
    [{ok, 0, get_cached_get_value()}];
handlerRpc({do, _Table, _Ops, _Flags, _Timeout}) ->
    not_implemented;
handlerRpc({add, _Table, _Key, _Val, _ExpTime, _Flags, _Timeout}) ->
    ok;
handlerRpc({replace, _Table, _Key, _Val, _ExpTime, _Flags, _Timeout}) ->
    ok;
handlerRpc({set, _Table, _Key, _Val, _ExpTime, _Flags, _Timeout}) ->
    ok;
handlerRpc({delete, _Table, _Key, _Flags, _Timeout}) ->
    ok;
handlerRpc({get, _Table, _Key, _Flags, _Timeout}) ->
    {ok, 0, get_cached_get_value()};
handlerRpc({get_many, _Table, _Key, _Num, _Flags, _Timeout}) ->
    not_implemented;
handlerRpc({brick_status, _Brick, _Node, _Timeout}) ->
    not_implemented;
handlerRpc(Event)
  when Event==info; Event==description ->
    ?S(?MODULE:Event());
handlerRpc(Event)
  when Event==keepalive ->
    ?MODULE:Event();
handlerRpc(Event) ->
    {Event, not_implemented}.


%%
%% helpers
%%

put_cached_get_value_size(Size) ->
    Key = {?MODULE,cached_get_value_size},
    erlang:put(Key, Size),
    ok.

get_cached_get_value() ->
    Key = {?MODULE,cached_get_value_size},
    Size =
        case erlang:get(Key) of
            undefined ->
                Val = 1000,
                erlang:put(Key, Val),
                Val;
            Val ->
                Val
        end,
    get_cached_get_value(Size).

get_cached_get_value(Size) ->
    Key = {?MODULE,cached_get_value,Size},
    case erlang:get(Key) of
        undefined ->
            random:seed(erlang:now()),
            Val = erlang:list_to_binary([ random:uniform(255) || _ <- lists:seq(1,Size) ]),
            erlang:put(Key, Val),
            Val;
        Val ->
            Val
    end.
