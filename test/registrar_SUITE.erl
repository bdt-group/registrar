%%%-------------------------------------------------------------------
%%% @author Evgeny Khramtsov <ekhramtsov@bdt.group>
%%% @copyright (C) 2020, Big Data Technology
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2020 by Evgeny Khramtsov <ekhramtsov@bdt.group>
%%%-------------------------------------------------------------------
-module(registrar_SUITE).

%% Note: This directive should only be used in test suites.
-compile(export_all).
-compile(nowarn_export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

%%--------------------------------------------------------------------
%% COMMON TEST CALLBACK FUNCTIONS
%%--------------------------------------------------------------------
suite() ->
    [{timetrap, {seconds, 20}}].

init_per_suite(Config) ->
    ?assertEqual(ok, registrar:start()),
    Config.

end_per_suite(_Config) ->
    ?assertEqual(ok, registrar:stop()).

init_per_group(Mod, Config) when Mod == registrar_local;
                                 Mod == registrar_gproc ->
    ?assertEqual(ok, Mod:start()),
    lists:keystore(module, 1, Config, {module, Mod});
init_per_group(registrar_local_named, Config) ->
    lists:keystore(module, 1, Config, {module, registrar_local});
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(Mod, _Config) when Mod == registrar_local;
                                 Mod == registrar_gproc ->
    ?assertEqual(ok, Mod:stop());
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    GeneralTests = [reg_unreg_case,
                    send_case,
                    double_reg_case,
                    reg_existent_case,
                    unreg_non_existent_case,
                    send_to_non_existent_case,
                    whereis_dead_case,
                    reg_dead_case,
                    double_start_stop_case],
    [{registrar_local, [sequence],
      GeneralTests ++
          [registrar_local_unexpected_msg_case,
           registrar_local_dead_cleanup_case]},
     {registrar_local_named, [sequence], [different_names_case]},
     {registrar_gproc, [sequence], GeneralTests}].

all() ->
    [{group, registrar_local},
     {group, registrar_local_named},
     {group, registrar_gproc}].

%%--------------------------------------------------------------------
%% General cases: these test cases are performed for all registrars
%%--------------------------------------------------------------------
reg_unreg_case(Config) ->
    Mod = ?config(module, Config),
    Self = self(),
    ?assertEqual(yes, Mod:register_name(?FUNCTION_NAME, Self)),
    ?assertEqual(Self, Mod:whereis_name(?FUNCTION_NAME)),
    ?assertEqual(ok, Mod:unregister_name(?FUNCTION_NAME)),
    ?assertEqual(undefined, Mod:whereis_name(?FUNCTION_NAME)),
    Config.

send_case(Config) ->
    Mod = ?config(module, Config),
    Self = self(),
    Msg = {make_ref(), msg},
    ?assertEqual(yes, Mod:register_name(?FUNCTION_NAME, Self)),
    ?assertEqual(Self, Mod:send(?FUNCTION_NAME, Msg)),
    receive
        Msg ->
            ?assertEqual(ok, Mod:unregister_name(?FUNCTION_NAME)),
            Config
    end.

double_reg_case(Config) ->
    Mod = ?config(module, Config),
    Self = self(),
    ?assertEqual(yes, Mod:register_name(?FUNCTION_NAME, Self)),
    ?assertEqual(no, Mod:register_name(?FUNCTION_NAME, Self)),
    ?assertEqual(ok, Mod:unregister_name(?FUNCTION_NAME)),
    Config.

reg_existent_case(Config) ->
    Mod = ?config(module, Config),
    Self = self(),
    ?assertEqual(yes, Mod:register_name(?FUNCTION_NAME, Self)),
    register_and_die(Mod, ?FUNCTION_NAME, no),
    ?assertEqual(ok, Mod:unregister_name(?FUNCTION_NAME)),
    Config.

unreg_non_existent_case(Config) ->
    Mod = ?config(module, Config),
    ?assertEqual(ok, Mod:unregister_name(?FUNCTION_NAME)),
    Config.

send_to_non_existent_case(Config) ->
    Mod = ?config(module, Config),
    Msg = {make_ref(), msg},
    ?assertError({badarg, {?FUNCTION_NAME, Msg}},
                 Mod:send(?FUNCTION_NAME, Msg)),
    Config.

whereis_dead_case(Config) ->
    Mod = ?config(module, Config),
    register_and_die(Mod, ?FUNCTION_NAME, yes),
    ?assertEqual(undefined, Mod:whereis_name(?FUNCTION_NAME)),
    Config.

reg_dead_case(Config) ->
    Mod = ?config(module, Config),
    Self = self(),
    register_and_die(Mod, ?FUNCTION_NAME, yes),
    ?assertEqual(yes, Mod:register_name(?FUNCTION_NAME, Self)),
    ?assertEqual(ok, Mod:unregister_name(?FUNCTION_NAME)),
    Config.

double_start_stop_case(Config) ->
    Mod = ?config(module, Config),
    ?assertEqual(ok, Mod:start()),
    ?assertEqual(ok, Mod:stop()),
    ?assertEqual(ok, Mod:stop()),
    ?assertEqual(ok, Mod:start()),
    Config.

different_names_case(Config) ->
    Mod = ?config(module, Config),
    Self = self(),
    Msg = {make_ref(), msg},
    ?assertEqual(ok, Mod:start(registrar1)),
    ?assertEqual(ok, Mod:start(registrar2)),
    ?assertEqual(yes, Mod:register_name({Mod, registrar1, ?FUNCTION_NAME}, Self)),
    ?assertEqual(yes, Mod:register_name({Mod, registrar2, ?FUNCTION_NAME}, Self)),
    ?assertEqual(Self, Mod:send({Mod, registrar1, ?FUNCTION_NAME}, Msg)),
    receive
        Msg -> ?assertEqual(ok, Mod:unregister_name({Mod, registrar1, ?FUNCTION_NAME}))
    end,
    ?assertEqual(undefined, Mod:whereis_name({Mod, registrar1, ?FUNCTION_NAME})),
    ?assertEqual(Self, Mod:send({Mod, registrar2, ?FUNCTION_NAME}, Msg)),
    receive
        Msg -> ?assertEqual(ok, Mod:unregister_name({Mod, registrar2, ?FUNCTION_NAME}))
    end,
    ?assertEqual(undefined, Mod:whereis_name({Mod, registrar2, ?FUNCTION_NAME})),
    Config.

%%--------------------------------------------------------------------
%% registrar_local specific test cases
%%--------------------------------------------------------------------
registrar_local_unexpected_msg_case(Config) ->
    Mod = ?config(module, Config),
    ?assertEqual(ok, gen_server:cast(Mod, test_unexpected_cast)),
    ?assertEqual(test_unexpected_info, Mod ! test_unexpected_info),
    ?assertExit({timeout, _}, gen_server:call(Mod, test_unexpected_call, 100)),
    Config.

registrar_local_dead_cleanup_case(Config) ->
    Mod = ?config(module, Config),
    ?assertEqual(ok, Mod:stop()),
    ?assertEqual(ok, Mod:start(#{clean_interval => 1000})),
    register_and_die(Mod, ?FUNCTION_NAME, yes),
    ct:sleep(2000),
    ?assertEqual(undefined, Mod:whereis_name(?FUNCTION_NAME)),
    Config.

%%--------------------------------------------------------------------
%% Misc
%%--------------------------------------------------------------------
register_and_die(Mod, Name, Match) ->
    {Pid, MRef} = erlang:spawn_monitor(
                    fun() ->
                            ?assertEqual(Match, Mod:register_name(Name, self()))
                    end),
    receive
        Msg ->
            ?assertEqual({'DOWN', MRef, process, Pid, normal}, Msg),
            ok
    end.
