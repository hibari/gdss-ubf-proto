-module(hibari_client).

-include("gen-erl/hibari_thrift.hrl").

-export([start/0]).

start() ->
    {ok, Client} = thrift_client:start_link("127.0.0.1", 7600, hibari_thrift),

%    % ping
%    Ping = thrift_client:call(Client, ping, []),
%    error_logger:info_msg("ping => ~p~n", [Ping]),
%
%    % info
%    Info = thrift_client:call(Client, info, []),
%    error_logger:info_msg("info => ~p~n", [Info]),
%
%    % desc
%    Desc = thrift_client:call(Client, description, []),
%    error_logger:info_msg("description => ~p~n", [Desc]),
%
%    % contract
%    Con = thrift_client:call(Client, contract, []),
%    error_logger:info_msg("contract => ~p~n", [Con]),
%
    % add
    Table1 = "tab1",
    Key1 = <<"key1">>,
    Key2 = <<"key2">>,
    Key3 = <<"key3">>,
    Value1 = <<"value1">>,
    Value2 = <<"value2">>,
    Value3 = <<"value3">>,

    % add - ok
    Add1_Param = #add{table=Table1, key=Key1, value=Value1},
    try
        Add1 = thrift_client:call(Client, 'Add', [Add1_Param]),
        error_logger:info_msg("Add ~p => ~p~n", [Add1_Param, Add1])
    catch
        Unexpectable ->
           error_logger:info_msg("*** Shouldn't be here! => ~p~n", [Unexpectable])
    end,

    % add - same key
    Add2_Param = #add{table=Table1, key=Key1, value=Value2},
    try
        Add2 = thrift_client:call(Client, 'Add', [Add2_Param]),
        error_logger:info_msg("*** wrong add ~p => ~p~n", [Add2_Param, Add2])
    catch
        All ->
            error_logger:info_msg("Add ~p catch ~p~n", [Add2_Param, All])
    end,

    % get - 
    Get1_Param = #get{table=Table1, key=Key1},
    try
        Get1 = thrift_client:call(Client, 'Get', [Get1_Param]),
        error_logger:info_msg("Get ~p => ~p~n", [Get1_Param, Get1])
    catch
        Unexpectable1 ->
           error_logger:info_msg("*** Shouldn't be here! => ~p~n", [Unexpectable1])
    end,

    % get - non-exist key
    Get2_Param = #get{table=Table1, key=Key2, is_witness=true},
    try
        Get2 = thrift_client:call(Client, 'Get', [Get2_Param]),
        error_logger:info_msg("*** wrong get ~p => ~p~n", [Get2_Param, Get2])
    catch
        All1 ->
           error_logger:info_msg("Get ~p catch ~p~n", [Get2_Param, All1])
    end,

    % replace
    Replace1_Param = #replace{table=Table1, key=Key1, value=Value2},
    try
        Replace1 = thrift_client:call(Client, 'Replace', [Replace1_Param]),
        error_logger:info_msg("Replace ~p => ~p~n", [Replace1_Param, Replace1])
    catch
        Unexpectable2 ->
           error_logger:info_msg("*** Shouldn't be here! => ~p~n", [Unexpectable2])
    end,

    % replace - non-exist key
    Replace2_Param = #replace{table=Table1, key=Key2, value=Value1},
    try
        Replace2 = thrift_client:call(Client, 'Replace', [Replace2_Param]),
        error_logger:info_msg("*** wrong replace ~p => ~p~n", [Replace2_Param, Replace2])
    catch
        All2 ->
            error_logger:info_msg("Replace ~p catch ~p~n", [Replace2_Param, All2])
    end,

    % set
    Set1_Param = #set{table=Table1, key=Key1, value=Value3},
    try
        Set1 = thrift_client:call(Client, 'Set', [Set1_Param]),
        error_logger:info_msg("Set ~p => ~p~n", [Set1_Param, Set1])
    catch
        Unexpectable3 ->
           error_logger:info_msg("*** Shouldn't be here! => ~p~n", [Unexpectable3])
    end,

    % set - non-exist key
    Set2_Param = #set{table=Table1, key=Key2, value=Value3},
    try
        Set2 = thrift_client:call(Client, 'Set', [Set2_Param]),
        error_logger:info_msg("Set ~p => ~p~n", [Set2_Param, Set2])
    catch
        All3 ->
            error_logger:info_msg("*** Shouldn't be here! ~p catch ~p~n", [Set2_Param, All3])
    end,


    % delete - non-exist key ok
    Delete1_Param = #delete{table=Table1, key=Key3},
    try
        Delete1 = thrift_client:call(Client, 'Delete', [Delete1_Param]),
        error_logger:info_msg("*** Shouldn't be here! delete non-exist key.~p~n", [Delete1])
    catch
        All4 ->
            error_logger:info_msg("Delete ~p catch ~p~n", [Delete1_Param, All4])
    end,

    % delete - non-exist key
    Delete2_Param = #delete{table=Table1, key=Key3, must_exist=false},
    try
        Delete2 = thrift_client:call(Client, 'Delete', [Delete2_Param]),
        error_logger:info_msg("Delete ~p catch ~p~n", [Delete2_Param, All5])
    catch
        All5 ->
            error_logger:info_msg("*** Shouldn't be here! delete non-exist key.~p~n", [Delete2])
    end,


    ok = thrift_client:close(Client),
    error_logger:info_msg("done.~n", []),
    ok.
