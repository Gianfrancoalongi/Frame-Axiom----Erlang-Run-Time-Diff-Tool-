-module(frame_axiom_tests).
-include_lib("eunit/include/eunit.hrl").

process_creation_diff_test() ->
    Ref = frame_axiom:snapshot(process),
    Pid = spawn_link(fun() -> receive _ -> ok end end),
    ?assertEqual([{created,Pid}],frame_axiom:diff(Ref,process)).
