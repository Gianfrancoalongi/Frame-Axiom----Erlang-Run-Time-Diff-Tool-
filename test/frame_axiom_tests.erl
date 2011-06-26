%%% @author Gianfranco <zenon@zen.home>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 24 Jun 2011 by Gianfranco <zenon@zen.home>
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

application_load_diff_test() ->
    application:unload(sasl),
    Ref = frame_axiom:snapshot(application),
    application:load(sasl),
    ?assertEqual([{loaded,sasl}],frame_axiom:diff(Ref,application,[load_unload])).

application_unload_diff_test() ->
    application:load(sasl),
    Ref = frame_axiom:snapshot(application),
    application:unload(sasl),
    ?assertEqual([{unloaded,sasl}],frame_axiom:diff(Ref,application,[load_unload])).

application_load_no_change_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    ?assertEqual([],frame_axiom:diff(Ref,application,[load_unload])).

ets_creation_diff_test() ->
    Ref = frame_axiom:snapshot(ets),
    New = ets:new(some,[]),
    ?assertEqual([{created,New}],frame_axiom:diff(Ref,ets)).

ets_delection_diff_test() ->
    E = ets:new(some,[]),
    Ref = frame_axiom:snapshot(ets),
    ets:delete(E),
    ?assertEqual([{deleted,E}],frame_axiom:diff(Ref,ets)).

ets_no_change_test() ->
    Ref = frame_axiom:snapshot(ets),
    ?assertEqual([],frame_axiom:diff(Ref,ets)).

ports_creation_diff_test() ->
    Ref = frame_axiom:snapshot(port),
    P = erlang:open_port({spawn,"cd"},[stream]),
    ?assertEqual([{opened,P}],frame_axiom:diff(Ref,port)),
    erlang:port_close(P).

ports_closing_diff_test() ->
    P = erlang:open_port({spawn,"cd"},[stream]),    
    Ref = frame_axiom:snapshot(port),
    erlang:port_close(P),
    ?assertEqual([{closed,P}],frame_axiom:diff(Ref,port)).

ports_no_change_diff_test() ->
    Ref = frame_axiom:snapshot(port),
    ?assertEqual([],frame_axiom:diff(Ref,port)).

file_direct_under_creation_diff_test() ->
    Path = ".",
    Ref = frame_axiom:snapshot({dir,Path}),
    Name= "created_this.txt",
    file:write_file(Name,"HelloWorld"),
    FilePath = filename:join(Path,Name),
    ?assertEqual([{created,{file,FilePath}}],frame_axiom:diff(Ref,{dir,Path})),
    file:delete(FilePath).

file_direct_under_deletion_diff_test() ->
    Path = ".",
    Name= "created_this.txt",
    file:write_file(Name,"HelloWorld"),
    Ref = frame_axiom:snapshot({dir,Path}),
    FilePath = filename:join(Path,Name),
    file:delete(FilePath),
    ?assertEqual([{deleted,{file,FilePath}}],frame_axiom:diff(Ref,{dir,Path})).

file_no_change_diff_test() ->
    Path = ".",
    Ref = frame_axiom:snapshot({dir,Path}),
    ?assertEqual([],frame_axiom:diff(Ref,{dir,Path})).

file_directory_creation_test() ->
    Path = ".",
    Name= "this_dir",
    Ref = frame_axiom:snapshot({dir,Path}),
    FullPath = filename:join(Path,Name),
    ok = filelib:ensure_dir(FullPath++"/"),
    ?assertEqual([{created,{dir,FullPath}}],frame_axiom:diff(Ref,{dir,Path})),
    file:del_dir(FullPath).

file_directory_deletion_test() ->
    Path = ".",
    Name= "this_dir",
    FullPath = filename:join(Path,Name),
    ok = filelib:ensure_dir(FullPath++"/"),
    Ref = frame_axiom:snapshot({dir,Path}),
    file:del_dir(FullPath),    
    ?assertEqual([{deleted,{dir,FullPath}}],frame_axiom:diff(Ref,{dir,Path})).    
    
multiple_type_creation_test() ->
    Ref = frame_axiom:snapshot([process,application,ets]),
    Pid = spawn_link(fun() -> receive _ -> ok end end),
    application:start(snmp),
    Ets = ets:new(created,[]),
    ?assertMatch([
		  {process,[{created,Pid},_,_,_]},
		  {application,[{started,snmp}]},
		  {ets,[{created,Ets}]}
		 ],
		 frame_axiom:diff(Ref,[process,
				       {application,[start_stop]},
				       ets])),
    application:stop(snmp),
    ets:delete(Ets),
    Pid ! die.
