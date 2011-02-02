%%% Copyright: (c) 2008-2011 Gemini Mobile Technologies, Inc.  All rights reserved.
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
%%% File    : ubf_gdss_plugin_eqc_tests.erl
%%% Purpose :
%%%----------------------------------------------------------------------

-module(ubf_gdss_plugin_eqc_tests).

-ifdef(EQC).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").

-include("ubf_gdss_plugin.hrl").

-export([run_tests_/0, run_tests/0, run_tests/1, run_parallel_tests/0, run_parallel_tests/1]).
-export([sample_commands/0, sample_commands/1, prop_commands/0, prop_commands/1]).
-export([counterexample_commands/0, counterexample_commands/1, counterexample_commands/2]).
-export([counterexample_commands_read/1, counterexample_commands_write/1, counterexample_commands_write/2]).

-export([ubf_command_contract/2, ubf_command_typename/3, ubf_command_typegen/6, ubf_command/5, ubf_command_custom/3]).
-export([ubf_initial_state/0, ubf_state_is_sane/1, ubf_next_state/3, ubf_precondition/2, ubf_postcondition/3]).
-export([ubf_commands_setup/1, ubf_commands_teardown/1, ubf_commands_teardown/2]).
-export([ubf_rpc/3]).

%%TODO: -behaviour(gmt_eqc_ubf).

-define(INIT_MOD, api_gdss_ubf_proto_init).

%%
%% TODO:
%%

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

%% external API
run_tests_() ->
    {timeout, 60, [fun() -> run_tests() end]}.

run_tests() ->
    run_tests(500).

run_tests(NumTests) ->
    eqc:module({numtests, NumTests}, ?MODULE).

run_parallel_tests() ->
    run_parallel_tests(500).

run_parallel_tests(NumTests) ->
    eqc:quickcheck(eqc:numtests(NumTests,noshrink(prop_commands([{parallel,true}])))).

%% sample commands
sample_commands() ->
    sample_commands([]).

sample_commands(Options) ->
    gmt_eqc_ubf:ubf_sample_commands(?MODULE, ?GDSS_PLUGINS, Options).

%% prop commands
prop_commands() ->
    prop_commands([]).

prop_commands(Options) ->
    gmt_eqc_ubf:ubf_run_commands(?MODULE, ?GDSS_PLUGINS, Options).

%% counterexample commands
counterexample_commands() ->
    counterexample_commands([]).

counterexample_commands(Options) ->
    counterexample_commands(Options, eqc:counterexample()).

counterexample_commands(Options, CounterExample) ->
    eqc:check(prop_commands(Options), CounterExample).

%% counterexample commands read
counterexample_commands_read(FileName) ->
    {ok, CounterExample} = file:consult(FileName),
    counterexample_commands(CounterExample).

%% counterexample commands write
counterexample_commands_write(FileName) ->
    counterexample_commands_write(FileName, eqc:counterexample()).

counterexample_commands_write(FileName, CounterExample) ->
    file:write_file(FileName, io_lib:format("~p.", [CounterExample])).

%%%----------------------------------------------------------------------
%%% CALLBACKS - gmt_eqc_ubf1
%%%----------------------------------------------------------------------

%% (S::symbolic_state(),Contracts::list(atom())) -> atom()
ubf_command_contract(_S,Contracts) ->
    oneof(Contracts).

%% (S::symbolic_state(),Contract::atom(),TypeNames::list(atom()) -> atom()
ubf_command_typename(_S,_Contract,TypeNames) ->
    oneof(TypeNames).

%% (Gen::fun(),Mod::atom(),S::symbolic_state(),Contract::atom(),TypeName::atom(),TypeStack::list()) -> gen()
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
    case ubf_client:lpc(Contract, Type, none, none) of
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

-endif. %% -ifdef(EQC).
