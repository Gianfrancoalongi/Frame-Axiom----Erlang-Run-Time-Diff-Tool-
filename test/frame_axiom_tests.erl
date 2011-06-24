-module(frame_axiom_tests).
-include_lib("eunit/include/eunit.hrl").

process_creation_diff_test() ->
    Ref = frame_axiom:snapshot(process),
    Pid = spawn_link(fun() -> receive _ -> ok end end),
    ?assertEqual([{created,Pid}],frame_axiom:diff(Ref,process)).

process_death_diff_test() ->
    process_flag(trap_exit,true),
    Pid = spawn_link(fun() -> receive _ -> ok end end),
    Ref = frame_axiom:snapshot(process),
    Pid ! die,
    receive
	{'EXIT',Pid,normal} -> ok_died
    end,
    ?assertEqual([{died,Pid}],frame_axiom:diff(Ref,process)).

process_no_change_diff_test() ->
    Ref = frame_axiom:snapshot(process),
    ?assertEqual([],frame_axiom:diff(Ref,process)).

application_creation_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    application:start(sasl),
    ?assertEqual([{started,sasl}],frame_axiom:diff(Ref,application,[start_stop])),
    application:stop(sasl).

application_stop_diff_test() ->
    application:start(sasl),
    Ref = frame_axiom:snapshot(application),
    application:stop(sasl),
    ?assertEqual([{stopped,sasl}],frame_axiom:diff(Ref,application,[start_stop])).

application_no_change_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    ?assertEqual([],frame_axiom:diff(Ref,application,[start_stop])).
