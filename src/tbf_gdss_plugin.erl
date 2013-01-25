%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2013 Hibari developers.  All rights reserved.
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
%%% File    : tbf_gdss_plugin.erl
%%% Purpose : native tbf gdss plugin implementation
%%%----------------------------------------------------------------------

-module(tbf_gdss_plugin).

-include("ubf.hrl").
-include("gmt_elog.hrl").

-export([info/0, description/0, keepalive/0]).
-export([handlerStart/1, handlerStop/3, handlerRpc/1]).
-export([debug/1, debug/2]).
-export([add/1, add/3, get/1, get/3, delete/1, delete/3]).
-export([replace/1, replace/3, set/1, set/3]).

%% NOTE defines and records are generated by thrift, see gdss-tbf-proto
-define(hibari_UNKNOWN, 1).
-define(hibari_UNKNOWN_ARGS, 2).
-define(hibari_SERVICE_NOT_AVAIL, 3).
-define(hibari_NOT_IMPLEMENTED, 4).
-define(hibari_TIME_OUT, 5).
-define(hibari_TS_ERROR, 6).
-define(hibari_KEY_EXISTS, 101).
-define(hibari_KEY_NOT_EXISTS, 102).

%% NOTE the following two lines
-compile({parse_transform,contract_parser}).
-add_contract("./src/tbf_gdss_plugin").

debug(Format, Data) ->
    ?ELOG_DEBUG(Format, Data).

debug(Message) ->
    ?ELOG_DEBUG(Message).

info() ->
    "I am a Hibari Server v.0.1".

description() ->
    "A Thrift server programmed by TBF".

contract() ->
    "TBD...".

keepalive() ->
    ok.


%% @spec handlerStart(Args::list(any())) ->
%%          {accept, Reply::any(), StateName::atom(), StateData::term()} | {reject, Reason::any()}
%% @doc start handler
handlerStart(_Args) ->
    {accept,ok,none,unused}.

%% @spec handlerStop(Pid::pid(), Reason::any(), StateData::term()) -> void()
%% @doc stop handler
handlerStop(_Pid, _Reason, _StateData) ->
    unused.


%% @spec handlerRpc(Event::any()) ->
%%          Reply::any()
%% @doc rpc handler
handlerRpc({message,<<"Add">>, 'T-CALL', SeqId, Params}) ->
    Param = get_first_param(Params),
    case add(Param) of
        {ok, Ts} ->
            FieldTimestamp = {field, <<>>, 'T-I64', 1, Ts},
            Response = {struct, <<>>, [FieldTimestamp]},
            Response1 = {field, <<>>, 'T-STRUCT', 0, Response},
            Responses = {struct, <<>>, [Response1]},
            {message, <<"Add">>, 'T-REPLY', SeqId, Responses};

        key_exists ->
            Exception = make_exception(?hibari_KEY_EXISTS, "Key Exist"),
            {message, <<"Add">>, 'T-REPLY', SeqId, Exception};

        brick_not_available ->
            Exception = make_exception(?hibari_SERVICE_NOT_AVAIL, "Brick Not Available"),
            {message, <<"Add">>, 'T-REPLY', SeqId, Exception};

        unknown_args ->
            Exception = make_exception(?hibari_UNKNOWN_ARGS, "Bad input params"),
            {message, <<"Add">>, 'T-REPLY', SeqId, Exception};

        timeout ->
            Exception = make_exception(?hibari_TIME_OUT, "Timeout"),
            {message, <<"Add">>, 'T-REPLY', SeqId, Exception};

        _ ->
            Exception = make_exception(?hibari_UNKNOWN, "Unknown"),
            {message, <<"Add">>, 'T-REPLY', SeqId, Exception}

    end;
handlerRpc({message,<<"Replace">>, 'T-CALL', SeqId, Params}) ->
    Param = get_first_param(Params),
    case replace(Param) of
        {ok, Ts} ->
            FieldTimestamp = {field, <<>>, 'T-I64', 1, Ts},
            Response = {struct, <<>>, [FieldTimestamp]},
            Response1 = {field, <<>>, 'T-STRUCT', 0, Response},
            Responses = {struct, <<>>, [Response1]},
            {message, <<"Replace">>, 'T-REPLY', SeqId, Responses};

        key_not_exist ->
            Exception = make_exception(?hibari_KEY_NOT_EXISTS, "Key Not Exist"),
            {message, <<"Replace">>, 'T-REPLY', SeqId, Exception};

        ts_error ->
            % input data is older than storage
            Exception = make_exception(?hibari_TS_ERROR, "TimeStamp Error"),
            {message, <<"Replace">>, 'T-REPLY', SeqId, Exception};

        brick_not_available ->
            Exception = make_exception(?hibari_SERVICE_NOT_AVAIL, "Brick Not Available"),
            {message, <<"Replace">>, 'T-REPLY', SeqId, Exception};

        unknown_args ->
            Exception = make_exception(?hibari_UNKNOWN_ARGS, "Bad input params"),
            {message, <<"Replace">>, 'T-REPLY', SeqId, Exception};

        timeout ->
            Exception = make_exception(?hibari_TIME_OUT, "Timeout"),
            {message, <<"Replace">>, 'T-REPLY', SeqId, Exception};

        _ ->
            Exception = make_exception(?hibari_UNKNOWN, "Unknown"),
            {message, <<"Replace">>, 'T-REPLY', SeqId, Exception}

    end;
handlerRpc({message,<<"Set">>, 'T-CALL', SeqId, Params}) ->
    Param = get_first_param(Params),
    case set(Param) of
        {ok, Ts} ->
            FieldTimestamp = {field, <<>>, 'T-I64', 1, Ts},
            Response = {struct, <<>>, [FieldTimestamp]},
            Response1 = {field, <<>>, 'T-STRUCT', 0, Response},
            Responses = {struct, <<>>, [Response1]},
            {message, <<"Set">>, 'T-REPLY', SeqId, Responses};

        brick_not_available ->
            Exception = make_exception(?hibari_SERVICE_NOT_AVAIL, "Brick Not Available"),
            {message, <<"Set">>, 'T-REPLY', SeqId, Exception};

        ts_error ->
            % input data is older than storage
            Exception = make_exception(?hibari_TS_ERROR, "TimeStamp Error"),
            {message, <<"Replace">>, 'T-REPLY', SeqId, Exception};

        unknown_args ->
            Exception = make_exception(?hibari_UNKNOWN_ARGS, "Bad input params"),
            {message, <<"Set">>, 'T-REPLY', SeqId, Exception};

        timeout ->
            Exception = make_exception(?hibari_TIME_OUT, "Timeout"),
            {message, <<"Set">>, 'T-REPLY', SeqId, Exception};

        _ ->
            Exception = make_exception(?hibari_UNKNOWN, "Unknown"),
            {message, <<"Set">>, 'T-REPLY', SeqId, Exception}

    end;
handlerRpc({message, <<"Delete">>, 'T-CALL', SeqId, Params}) ->
    Param = get_first_param(Params),
    case delete(Param) of
        {ok, Ts} ->
            FieldTimestamp = {field, <<>>, 'T-I64', 1, Ts},
            Response = {struct, <<>>, [FieldTimestamp]},
            Response1 = {field, <<>>, 'T-STRUCT', 0, Response},
            Responses = {struct, <<>>, [Response1]},
            {message, <<"Delete">>, 'T-REPLY', SeqId, Responses};

        key_not_exist ->
            Exception = make_exception(?hibari_KEY_NOT_EXISTS, "Key Not Exist"),
            {message, <<"Delete">>, 'T-REPLY', SeqId, Exception};

        brick_not_available ->
            Exception = make_exception(?hibari_SERVICE_NOT_AVAIL, "Brick Not Available"),
            {message, <<"Delete">>, 'T-REPLY', SeqId, Exception};

        unknown_args ->
            Exception = make_exception(?hibari_UNKNOWN_ARGS, "Bad input params"),
            {message, <<"Delete">>, 'T-REPLY', SeqId, Exception};

        timeout ->
            Exception = make_exception(?hibari_TIME_OUT, "Timeout"),
            {message, <<"Delete">>, 'T-REPLY', SeqId, Exception};

        _ ->
            Exception = make_exception(?hibari_UNKNOWN, "Unknown"),
            {message, <<"Delete">>, 'T-REPLY', SeqId, Exception}
    end;
handlerRpc({message, <<"Get">>, 'T-CALL', SeqId, Params}) ->
    Param = get_first_param(Params),
    case ?MODULE:get(Param) of
        {ok, Key, Ts, Value, _TimeElapsed} ->
            FieldTimestamp = {field, <<>>, 'T-I64', 1, Ts},
            FieldKey = {field, <<>>, 'T-BINARY', 2, Key},
            FieldValue = {field, <<>>, 'T-BINARY', 3, Value},
            Response = {struct, <<>>, [FieldTimestamp, FieldKey, FieldValue]},
            Response1 = {field, <<>>, 'T-STRUCT', 0, Response},
            Responses = {struct, <<>>, [Response1]},
            {message, <<"Get">>, 'T-REPLY', SeqId, Responses};

        key_not_exist ->
            Exception = make_exception(?hibari_KEY_NOT_EXISTS, "Key Not Exist"),
            {message, <<"Get">>, 'T-REPLY', SeqId, Exception};

        brick_not_available ->
            Exception = make_exception(?hibari_SERVICE_NOT_AVAIL, "Brick Not Available"),
            {message, <<"Get">>, 'T-REPLY', SeqId, Exception};

        unknown_args ->
            Exception = make_exception(?hibari_UNKNOWN_ARGS, "Bad input params"),
            {message, <<"Get">>, 'T-REPLY', SeqId, Exception};

        timeout ->
            Exception = make_exception(?hibari_TIME_OUT, "Timeout"),
            {message, <<"Get">>, 'T-REPLY', SeqId, Exception};

        _ ->
            Exception = make_exception(?hibari_UNKNOWN, "Unknown"),
            {message, <<"Get">>, 'T-REPLY', SeqId, Exception}
    end;
handlerRpc({message,<<"keepalive">>, 'T-CALL', SeqId, _NoArg}) ->
    %% TODO(gki): no_reply
    {message, <<"keepalive">>, 'T-REPLY', SeqId, {struct, <<>>, []}};
handlerRpc({message,<<"contract">>,'T-CALL',SeqId,_NoArg}) ->
    Field = {field, <<>>, 'T-BINARY', 0, list_to_binary(contract())},
    Struct = {struct, <<>>, [Field]},
    {message, <<"contract">>, 'T-REPLY', SeqId, Struct};
handlerRpc({message,<<"description">>,'T-CALL',SeqId,_NoArg}) ->
    Field = {field, <<>>, 'T-BINARY', 0, list_to_binary(description())},
    Struct = {struct, <<>>, [Field]},
    {message, <<"description">>, 'T-REPLY', SeqId, Struct};
handlerRpc({message,<<"info">>,'T-CALL',SeqId,_NoArg}) ->
    Field = {field, <<>>, 'T-BINARY', 0, list_to_binary(info())},
    Struct = {struct, <<>>, [Field]},
    {message, <<"info">>, 'T-REPLY', SeqId, Struct};
handlerRpc(Event) ->
    debug("Unknown rpc request [~p]", [Event]),
    {Event, not_implemented}.


%%% ----------------------------------------
%%% Private
%%% ----------------------------------------
add({struct, <<>>, Args}) when is_list(Args) ->
    {field, _, 'T-BINARY', 1, TableInBinary} = lists:keyfind(1, 4, Args),
    Table = gmt_util:atom_ify(TableInBinary),
    {field, _, 'T-BINARY', 2, Key}   = lists:keyfind(2, 4, Args),
    {field, _, 'T-BINARY', 3, Value} = lists:keyfind(3, 4, Args),
    add(Table, Key, Value);
add(_) ->
    unknown_args.

add(Table, Key, Value) ->
    StartTime = erlang:now(),
    case catch brick_simple:add(Table, Key, Value) of
        ok ->
            {ok, timer:now_diff(erlang:now(), StartTime)};

        {key_exists, _} ->
            key_exists;

        {txn_fail, [{_Integer, brick_not_available}]} ->
            debug("brick_simple:add [~p]~n", [brick_not_available]),
            brick_not_available;

        {'EXIT', {timeout, _}} ->
            timeout;

        Unknown ->
            debug("brick_simple:add [~p]~n", [Unknown]),
            Unknown
    end.


replace({struct, <<>>, Args}) when is_list(Args) ->
    {field, _, 'T-BINARY', 1, TableInBinary} = lists:keyfind(1, 4, Args),
    Table = gmt_util:atom_ify(TableInBinary),
    {field, _, 'T-BINARY', 2, Key}   = lists:keyfind(2, 4, Args),
    {field, _, 'T-BINARY', 3, Value} = lists:keyfind(3, 4, Args),
    replace(Table, Key, Value);
replace(_) ->
    unknown_args.

replace(Table, Key, Value) ->
    StartTime = erlang:now(),
    case catch brick_simple:replace(Table, Key, Value) of
        ok ->
            {ok, timer:now_diff(erlang:now(), StartTime)};

        key_not_exist ->
            key_not_exist;

        {txn_fail, [{_Integer, brick_not_available}]} ->
            debug("brick_simple:replace [~p]~n", [brick_not_available]),
            brick_not_available;

        {'EXIT', {timeout, _}} ->
            timeout;

        Unknown ->
            debug("brick_simple:replace [~p]~n", [Unknown]),
            Unknown
    end.

set({struct, <<>>, Args}) when is_list(Args) ->
    {field, _, 'T-BINARY', 1, TableInBinary} = lists:keyfind(1, 4, Args),
    Table = gmt_util:atom_ify(TableInBinary),
    {field, _, 'T-BINARY', 2, Key}   = lists:keyfind(2, 4, Args),
    {field, _, 'T-BINARY', 3, Value} = lists:keyfind(3, 4, Args),
    ?MODULE:set(Table, Key, Value);
set(_) ->
    unknown_args.

set(Table, Key, Value) ->
    StartTime = erlang:now(),
    case catch brick_simple:set(Table, Key, Value) of
        ok ->
            {ok, timer:now_diff(erlang:now(), StartTime)};

        {ts_error, _} ->
            ts_error;

        {txn_fail, [{_Integer, brick_not_available}]} ->
            debug("brick_simple:set [~p]~n", [brick_not_available]),
            brick_not_available;

        {'EXIT', {timeout, _}} ->
            timeout;

        Unknown ->
            debug("brick_simple:set [~p]~n", [Unknown]),
            Unknown
    end.

delete({struct, <<>>, Args}) when is_list(Args) ->
    {field, _, 'T-BINARY',  1, TableInBinary} = lists:keyfind(1, 4, Args),
    Table = gmt_util:atom_ify(TableInBinary),

    {field, _, 'T-BINARY',  2, Key} = lists:keyfind(2, 4, Args),
    MustExist = case lists:keyfind(3, 4, Args) of
                    {field, _, 'T-BOOL', 3, Arg3} ->
                        Arg3;
                    _ ->
                        undefined
                end,

    Flags = case MustExist of
                true -> [must_exist];
                _ -> []
            end,

    delete(Table, Key, Flags);
delete(_) ->
    unknown_args.

delete(Table, Key, Flags) ->
    StartTime = erlang:now(),
    case catch brick_simple:delete(Table, Key, Flags) of
        ok ->
            {ok, timer:now_diff(erlang:now(), StartTime)};

        key_not_exist ->
            key_not_exist;

        {txn_fail, [{_Integer, brick_not_available}]} ->
            debug("brick_simple:delete [~p]~n", [brick_not_available]),
            brick_not_available;

        {'EXIT', {timeout, _}} ->
            timeout;

        Unknown ->
            debug("brick_simple:delete [~p]~n", [Unknown]),
            Unknown
    end.


get({struct, <<>>, Args}) when is_list(Args) ->
    {field, _, 'T-BINARY',  1, TableInBinary} = lists:keyfind(1, 4, Args),
    Table = gmt_util:atom_ify(TableInBinary),

    {field, _, 'T-BINARY',  2, Key} = lists:keyfind(2, 4, Args),

    IsWitness = case lists:keyfind(3, 4, Args) of
                    {field, _, 'T-BOOL', 3, Arg3} ->
                        Arg3;
                    _ ->
                        undefined
                end,

    Flags = case IsWitness of
                true -> [witness];
                _ -> []
            end,
    ?MODULE:get(Table, Key, Flags);
get(_) ->
    unknown_args.

get(Table, Key, Flags) ->
    StartTime = erlang:now(),
    case catch brick_simple:get(Table, Key, Flags) of
        {ok, Timestamp, Value} ->
            {ok, Key, Timestamp, Value, timer:now_diff(erlang:now(), StartTime)};

        key_not_exist ->
            key_not_exist;

        {txn_fail, [{_Integer, brick_not_available}]} ->
            debug("brick_simple:get [~p]~n", [brick_not_available]),
            brick_not_available;

        {'EXIT', {timeout, _}} ->
            timeout;

        Unknown ->
            debug("brick_simple:get [~p]~n", [Unknown]),
            Unknown
    end.


%%% ----------------------------------------
%%% Helpers - to reduce duplicated code
%%% ----------------------------------------
make_exception(What, Why) when is_integer(What), is_list(Why) ->
    Exception = {struct, <<>>, [
                                {field, <<>>, 'T-I32', 2, What},
                                {field, <<>>, 'T-BINARY', 3, list_to_binary(Why)}
                               ]},
    Response1 = {field, <<>>, 'T-STRUCT', 1, Exception},
    Responses = {struct, <<>>, [Response1]},
    Responses.

get_first_param(Params) ->
    {struct, _, [Param]} = Params,
    {field, _, 'T-STRUCT', 1, Param1} = Param,
    Param1.
