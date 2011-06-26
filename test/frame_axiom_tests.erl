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
    application:start(snmp),
    ?assertEqual([{started,snmp}],frame_axiom:diff(Ref,application,[start_stop])),
    application:stop(snmp).

application_stop_diff_test() ->
    application:start(snmp),
    Ref = frame_axiom:snapshot(application),
    application:stop(snmp),
    ?assertEqual([{stopped,snmp}],frame_axiom:diff(Ref,application,[start_stop])).

application_no_change_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    ?assertEqual([],frame_axiom:diff(Ref,application,[start_stop])).

application_load_diff_test() ->
    application:unload(snmp),
    Ref = frame_axiom:snapshot(application),
    application:load(snmp),
    ?assertEqual([{loaded,snmp}],frame_axiom:diff(Ref,application,[load_unload])).

application_unload_diff_test() ->
    application:load(snmp),
    Ref = frame_axiom:snapshot(application),
    application:unload(snmp),
    ?assertEqual([{unloaded,snmp}],frame_axiom:diff(Ref,application,[load_unload])).

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

multiple_type_deletion_test() ->
    Pid = spawn_link(fun() -> receive _ -> ok end end),
    application:start(snmp),
    Ets = ets:new(created,[]),
    Ref = frame_axiom:snapshot([process,application,ets]),
    application:stop(snmp),
    ets:delete(Ets),
    Pid ! die,
    receive
	{'EXIT',Pid,normal} -> ok
    end,
    ?assertMatch([
		  {process,[{died,Pid},_,_,_]},
		  {application,[{stopped,snmp}]},
		  {ets,[{deleted,Ets}]}
		 ],
		 frame_axiom:diff(Ref,[process,
				       {application,[start_stop]},
				       ets])).
    
multiple_type_mixed_test() ->
    Ets = ets:new(created,[]),
    Ref = frame_axiom:snapshot([process,ets]),
    Pid = spawn_link(fun() -> receive _ -> ok end end),
    ets:delete(Ets),
    ?assertMatch([
		  {process,[{created,Pid}]},
		  {ets,[{deleted,Ets}]}
		 ],
		 frame_axiom:diff(Ref,[process,ets])),
    Pid ! die.

multiple_type_no_change_test() ->
    Ref = frame_axiom:snapshot([process,ets]),
    ?assertEqual([{process,[]},{ets,[]}],frame_axiom:diff(Ref,[process,ets])).

second_snapshot_call_test() ->
    Ref = frame_axiom:snapshot(ets),
    Ets = ets:new(test,[]),
    frame_axiom:snapshot(Ref,application),
    application:start(snmp),
    ?assertEqual([{ets,[{created,Ets}]},
		  {application,[{started,snmp}]}
		 ],
		 frame_axiom:diff(Ref,[ets,{application,[start_stop]}])).
    

node_connected_diff_test() ->
    ensure_empd(),
    net_kernel:start([box_box,shortnames]),
    Ref = frame_axiom:snapshot(node),
    {ok,Node} = slave:start(list_to_atom(inet_db:gethostname()),'slave'),
    ?assertEqual([{connected,Node}],frame_axiom:diff(Ref,node)),
    slave:stop(Node),
    net_kernel:stop().

node_disconnected_diff_test() ->
    ensure_empd(),
    net_kernel:start([box_box,shortnames]),
    {ok,Node} = slave:start(list_to_atom(inet_db:gethostname()),'slave'),
    Ref = frame_axiom:snapshot(node),
    slave:stop(Node),
    ?assertEqual([{disconnected,Node}],frame_axiom:diff(Ref,node)),
    net_kernel:stop().

ensure_empd() ->
    EPMD = os:cmd("epmd -names"),
    case {re:run(EPMD,"epmd: Cannot connect to local epmd.*"),
	  re:run(EPMD,"epmd: up and running on port .*")} of
	{{match,_},nomatch} ->
	    os:cmd("epmd -daemon");
	{nomatch,{match,_}} -> ok
    end.

successive_snapshots_of_same_resets_test() ->    
    Ref = frame_axiom:snapshot(ets),
    Ets = ets:new(a,[]),
    frame_axiom:snapshot(Ref,ets),
    ?assertEqual([],frame_axiom:diff(Ref,ets)),
    ets:delete(Ets).

named_process_creation_diff_test() ->
    Ref = frame_axiom:snapshot(named_process),    
    Pid = spawn_link(fun() -> register(this_named,self()), receive _ -> ok end end),
    ?assertEqual([{created,this_named}],frame_axiom:diff(Ref,named_process)),
    Pid ! die.

named_process_dying_diff_test() ->
    Pid = spawn_link(fun() -> register(this_named,self()), receive _ -> ok end end),
    Ref = frame_axiom:snapshot(named_process),    
    Pid ! die,
    receive
	{'EXIT',Pid,normal} -> ok
    end,
    ?assertEqual([{died,this_named}],frame_axiom:diff(Ref,named_process)).
    
named_process_no_diff_test() ->
    Ref = frame_axiom:snapshot(named_process),    
    ?assertEqual([],frame_axiom:diff(Ref,named_process)).
    
