%%%----------------------------------------------------------------------
%%% Copyright (c) 2008-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%-export([do/1, do/3]).

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
handlerRpc({message, <<"Do">>, 'T-CALL', SeqId, Params}) ->
    io:format("Do called~n"),
    Res = do(Params),
    io:format("Do: ~p~n", [Res]),

    Response = {struct, <<>>, []},
    Response1 = {field, <<>>, 'T-STRUCT', 0, Response},
    Responses = {struct, <<>>, [Response1]},
    {message, <<"Do">>, 'T-REPLY', SeqId, Responses};
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

%% {message,<<"Do">>,'T-CALL',6,
%%                   {struct,<<>>,
%%                       [{field,<<>>,'T-STRUCT',1,
%%                            {struct,<<>>,
%%                                [{field,<<>>,'T-BINARY',1,<<"tab1">>},
%%                                 {field,<<>>,'T-LIST',2,
%%                                     {list,'T-STRUCT',[]}}]}}]}}

do({struct, <<>>, [{field, <<>>, 'T-STRUCT', 1, {struct, <<>>, Args}}]}) when is_list(Args) ->
    {field, _, 'T-BINARY', 1, TableInBinary} = lists:keyfind(1, 4, Args),
    _Table = gmt_util:atom_ify(TableInBinary),
    {field, _, 'T-LIST', 2, {list, 'T-STRUCT', OpList}} = lists:keyfind(2, 4, Args),

    DoOps = lists:map(fun make_do_op/1, OpList),
    DoOps;
    %%do(DoOps);
do(DoOps) when is_list(DoOps) ->
    StartTime = erlang:now(),
    case catch brick_simple:do(DoOps) of
        ok ->
            {ok, timer:now_diff(erlang:now(), StartTime)};

%%        {key_exists, _} ->
%%            key_exists;

%%        {txn_fail, [{_Integer, brick_not_available}]} ->
%%            debug("brick_simple:add [~p]~n", [brick_not_available]),
%%            brick_not_available;

        {'EXIT', {timeout, _}} ->
            timeout;

        Unknown ->
            debug("brick_simple:do [~p]~n", [Unknown]),
            Unknown
    end;
do(_) ->
    unknown_args.

-define(hibari_DO_TXN, 1).
-define(hibari_DO_ADD, 2).
-define(hibari_DO_REPLACE, 3).
-define(hibari_DO_SET, 4).
-define(hibari_DO_DELETE, 5).
-define(hibari_DO_GET, 6).
-define(hibari_DO_GET_MANY, 7).

make_do_op(Op) ->
    {N, Param} = get_n_param(Op),
    {struct, <<>>, Args} = Param,
    case N of
        ?hibari_DO_TXN ->
            brick_server:make_txn();
        ?hibari_DO_ADD ->
            {Key, Value} = parse_set_args(Args, 0),
            brick_server:make_add(Key, Value);
        ?hibari_DO_REPLACE ->
            {Key, Value} = parse_set_args(Args, 0),
            brick_server:make_replace(Key, Value);
        ?hibari_DO_SET ->
            {Key, Value} = parse_set_args(Args, 0),
            brick_server:make_set(Key, Value);
        ?hibari_DO_DELETE ->
            {Key, Flags} = parse_delete_args(Args, 0),
            brick_server:make_delete(Key, Flags);
        ?hibari_DO_GET ->
            {_Table, Key, Flags} = parse_get_args(Args),
            brick_server:make_get(Key, Flags);
        ?hibari_DO_GET_MANY ->
            get_many
    end.


%%               {message,<<"Do">>,'T-CALL',6,
%%                {struct,<<>>,
%%                 [{field,<<>>,'T-STRUCT',1,
%%                   {struct,<<>>,
%%                    [{field,<<>>,'T-BINARY',1,<<"tab1">>},
%%                     {field,<<>>,'T-LIST',2,
%%                      {list,'T-STRUCT',
%%                       [{struct,<<>>,
%%                         [{field,<<>>,'T-STRUCT',1,{struct,<<>>,[]}}]},
%%                        {struct,<<>>,
%%                         [{field,<<>>,'T-STRUCT',2,
%%                           {struct,<<>>,
%%                            [{field,<<>>,'T-BINARY',1,<<"yo-key">>},
%%                             {field,<<>>,'T-BINARY',2,<<"Yo!">>}]}}]},
%%                        {struct,<<>>,
%%                         [{field,<<>>,'T-STRUCT',2,
%%                           {struct,<<>>,
%%                            [{field,<<>>,'T-BINARY',1,<<"ho-key">>},
%%                             {field,<<>>,'T-BINARY',2,<<"Ho!">>}]}}]},
%%                        {struct,<<>>,
%%                         [{field,<<>>,'T-STRUCT',3,
%%                           {struct,<<>>,
%%                            [{field,<<>>,'T-BINARY',1,<<"yo-key">>},
%%                             {field,<<>>,'T-BINARY',2,<<"Yo-yo!">>}]}}]},
%%                        {struct,<<>>,
%%                         [{field,<<>>,'T-STRUCT',5,
%%                           {struct,<<>>,
%%                            [{field,<<>>,'T-BINARY',1,<<"yo-key">>}]}}]},
%%                        {struct,<<>>,
%%                         [{field,<<>>,'T-STRUCT',5,
%%                           {struct,<<>>,
%%                            [{field,<<>>,'T-BINARY',1,
%%                              <<"ho-key">>}]}}]}]}}]}}]}}


%%% ----------------------------------------
%%% Private
%%% ----------------------------------------
add({struct, <<>>, Args}) when is_list(Args) ->
    Table = parse_table_name(Args),
    {Key, Value} = parse_set_args(Args, 1),
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

parse_set_args(Args, Offset) ->
    {field, _, 'T-BINARY', _, Key}   = lists:keyfind(Offset + 1, 4, Args),
    {field, _, 'T-BINARY', _, Value} = lists:keyfind(Offset + 2, 4, Args),
    {Key, Value}.

replace({struct, <<>>, Args}) when is_list(Args) ->
    Table = parse_table_name(Args),
    {Key, Value} = parse_set_args(Args, 1),
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
    Table = parse_table_name(Args),
    {Key, Value} = parse_set_args(Args, 1),
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
    Table = parse_table_name(Args),
    {Key, Flags} = parse_delete_args(Args, 1),
    delete(Table, Key, Flags);
delete(_) ->
    unknown_args.

parse_table_name(Args) ->
    {field, _, 'T-BINARY',  1, TableInBinary} = lists:keyfind(1, 4, Args),
    gmt_util:atom_ify(TableInBinary).

parse_delete_args(Args, Offset) ->
    {field, _, 'T-BINARY',  _, Key} = lists:keyfind(Offset + 1, 4, Args),
    MustExist = case lists:keyfind(Offset + 2, 4, Args) of
                    {field, _, 'T-BOOL', _, Arg3} ->
                        Arg3;
                    _ ->
                        undefined
                end,

    Flags = case MustExist of
                true -> [must_exist];
                _ -> []
            end,

    {Key, Flags}.

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
    {Table, Key, Flags} = parse_get_args(Args),
    ?MODULE:get(Table, Key, Flags);
get(_) ->
    unknown_args.

parse_get_args(Args) ->
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

    {Table, Key, Flags}.

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

get_n_param(Params) ->
    {struct, _, [Param]} = Params,
    {field, _, 'T-STRUCT', N, Param1} = Param,
    {N, Param1}.
