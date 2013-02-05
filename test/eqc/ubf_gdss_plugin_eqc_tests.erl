%%% Copyright: (c) 2008-2013 Hibari developers.  All rights reserved.
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
%%% File    : ubf_gdss_plugin_eqc.erl
%%% Purpose :
%%%----------------------------------------------------------------------

-module(ubf_gdss_plugin_eqc_tests).

-ifdef(PROPER).
-include_lib("proper/include/proper.hrl").
-define(GMTQC, proper).
-undef(EQC).
-endif. %% -ifdef(PROPER).

-ifdef(EQC).
-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-define(GMTQC, eqc).
-undef(PROPER).
-endif. %% -ifdef(EQC).

-ifdef(GMTQC).

-include("ubf_gdss_plugin.hrl").

%% DISABLE -export([eunit_test_/0, run/0, run/1, run_parallel/0, run_parallel/1]).
-export([run/0, run/1, run_parallel/0, run_parallel/1]).
-export([sample_commands/0, sample_commands/1, prop_commands/0, prop_commands/1]).
-export([counterexample_commands/0, counterexample_commands/1, counterexample_commands/2]).
-export([counterexample_commands_read/1, counterexample_commands_write/1, counterexample_commands_write/2]).

-export([ubf_command_contract/2, ubf_command_typename/3, ubf_command_typegen/6, ubf_command/5, ubf_command_custom/3]).
-export([ubf_initial_state/0, ubf_state_is_sane/1, ubf_next_state/3, ubf_precondition/2, ubf_postcondition/3]).
-export([ubf_commands_setup/1, ubf_commands_teardown/1, ubf_commands_teardown/2]).
-export([ubf_rpc/3]).

%% testing
-export([tabgen/0, keygen/0, keypartgen/0]).

%%TODO: -behaviour(gmt_eqc_ubf).

-define(INIT_MOD, ubf_gdss_eunit_utils).

%% R13 filename implementation
-ifdef(old_filename).
join(Parts) ->
    Parts1 = [ binary_to_list(X) || X <- Parts ],
    list_to_binary(filename:join(Parts1)).
-else.
-import(filename, [join/1]).
-endif.


%%%----------------------------------------------------------------------
%%% records
%%%----------------------------------------------------------------------

%%%%%%
%% state
-record(state, {
          parallel=false
         }).


%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------

%% run from eunit
%% DISABLE eunit_test_() ->
%% DISABLE    gmt_eqc:eunit_module(?MODULE, 5).

run() ->
    run(500).

run(NumTests) ->
    gmt_eqc:module({numtests,NumTests}, ?MODULE).

run_parallel() ->
    run_parallel(500).

run_parallel(NumTests) ->
    ?GMTQC:quickcheck(numtests(NumTests,noshrink(prop_commands([{parallel,true}])))).

%% sample commands
sample_commands() ->
    sample_commands([]).

sample_commands(Options) ->
    qc_ubf:ubf_sample_commands(?MODULE, ?GDSS_PLUGINS, Options).

%% prop commands
prop_commands() ->
    prop_commands([]).

prop_commands(Options) ->
    qc_ubf:ubf_run_commands(?MODULE, ?GDSS_PLUGINS, Options).

%% counterexample commands
counterexample_commands() ->
    counterexample_commands([]).

counterexample_commands(Options) ->
    counterexample_commands(Options, ?GMTQC:counterexample()).

counterexample_commands(Options, CounterExample) ->
    ?GMTQC:check(prop_commands(Options), CounterExample).

%% counterexample commands read
counterexample_commands_read(FileName) ->
    {ok, CounterExample} = file:consult(FileName),
    counterexample_commands(CounterExample).

%% counterexample commands write
counterexample_commands_write(FileName) ->
    counterexample_commands_write(FileName, ?GMTQC:counterexample()).

counterexample_commands_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.", [CounterExample])).

%%%----------------------------------------------------------------------
%%% CALLBACKS - eqc_ubf1
%%%----------------------------------------------------------------------

%% (S::symbolic_state(),Contracts::list(atom())) -> atom()
ubf_command_contract(_S,Contracts) ->
    oneof(Contracts).

%% (S::symbolic_state(),Contract::atom(),TypeNames::list(atom()) -> atom()
ubf_command_typename(_S,_Contract,TypeNames) ->
    oneof(TypeNames).

%% (Gen::fun(),Mod::atom(),S::symbolic_state(),Contract::atom(),TypeName::atom(),TypeStack::list()) -> gen()
ubf_command_typegen(Gen,Mod,S,Contract,table=TypeName,TypeStack) ->
    frequency([{9, tabgen()}                                 %% 90% existing table
               , {1, Gen(Mod,S,Contract,TypeName,TypeStack)} %% 10% default generator
              ]);
ubf_command_typegen(Gen,Mod,S,Contract,key=TypeName,TypeStack) ->
    frequency([{9, keygen()}                                 %% 90%
               , {1, Gen(Mod,S,Contract,TypeName,TypeStack)} %% 10% default generator
              ]);
ubf_command_typegen(_Gen,_Mod,_S,_Contract,timeout=_TypeName,_TypeStack) ->
    gmt_eqc_gen:gmt_timeout([noninfinite]);
ubf_command_typegen(Gen,Mod,S,Contract,TypeName,TypeStack) ->
    %% DEBUG io:format("~p ~p~n", [TypeName, TypeStack]),
    Gen(Mod,S,Contract,TypeName,TypeStack).

%% (Gen::fun(),Mod::atom(),S::symbolic_state(),Contract::atom(),TypeName::atom()) -> gen()
ubf_command(Gen,Mod,S,Contract,TypeName) ->
    %% DEBUG io:format("~p ~p~n", [TypeName]),
    %% resize(15,
    ?LET(Type,Gen(Mod,S,Contract,TypeName,[]),
         {call,?MODULE,ubf_rpc,[Contract,TypeName,Type]})
    %% )
        .

%% (Gen::fun(),Mod::atom(),S::symbolic_state()) -> gen()
ubf_command_custom(_Gen,_Mod,_S) ->
    exit(not_implemented).

%% (Contract::atom(),TypeName::atom(),Type::tuple()) -> term()
ubf_rpc(Contract,_TypeName,Type) ->
    case ubf_client:lpc(Contract, Type) of
        {reply,Reply,none} ->
            Reply;
        Err ->
            erlang:error(Err)
    end.

%% ubf_initial_state -> symbolic_state()
ubf_initial_state() ->
    ?LET(Parallel,parameter(parallel),
         #state{parallel=Parallel}).

%% ubf_state_is_sane(S::symbolic_state()) -> bool()
ubf_state_is_sane(_S) ->
    true.

%% ubf_next_state(S::symbolic_state(),R::var(),C::call()) -> symbolic_state()
ubf_next_state(S,_R,{call,_,ubf_rpc,[_Contract,_TypeName,_Type]}) ->
    S.

%% ubf_precondition(S::symbolic_state(),C::call()) -> bool()
ubf_precondition(_S,{call,_,ubf_rpc,[_Contract,_TypeName,_Type]}=_C) ->
    true;
ubf_precondition(_S,C) ->
    io:format("Precondition error: ~p~n", [C]),
    false.

%% ubf_postcondition(S::dynamic_state(),C::call(),R::term()) -> bool()
ubf_postcondition(_S,{call,_,ubf_rpc,[_Contract,_TypeName,_Type]}=_C,_R) ->
    true;
ubf_postcondition(_S,C,R) ->
    io:format("Postcondition error: ~p -> ~p~n", [C, R]),
    false.

%% @doc setup helper
ubf_commands_setup(Hard) ->
    %% reset
    if Hard ->
            ?INIT_MOD:simple_hard_reset();
       true ->
            ?INIT_MOD:simple_soft_reset()
    end,
    {ok,noop}.

%% @doc teardown helper
ubf_commands_teardown(_) ->
    ?INIT_MOD:simple_soft_reset(),
    ok.

%% @doc teardown helper
ubf_commands_teardown(_,_S) ->
    ?INIT_MOD:simple_soft_reset(),
    ok.

%%%----------------------------------------------------------------------
%%% INTERNAL
%%%----------------------------------------------------------------------

tables() ->
    [Tab || {Tab,_,_} <- ubf_gdss_eunit_utils:all_tables() ].

tabgen() ->
    elements(tables()).

keygen() ->
    ?LET(N, choose(0, 4),
         ?LET(Parts, vector(N, keypartgen()),
              case Parts of
                  [] ->
                      <<>>;
                  _ ->
                      ?LET(Root, frequency([{9, <<"/">>}, {1, <<>>}]),
                           iolist_to_binary([Root, join(Parts)]))
              end)).

keypartgen() ->
    frequency([{9, elements([<<>>, <<"a">>, <<"b">>, <<"c">>, <<"d">>])}
               , {1, binary()}
              ]).

-endif. %% -ifdef(GMTQC).
