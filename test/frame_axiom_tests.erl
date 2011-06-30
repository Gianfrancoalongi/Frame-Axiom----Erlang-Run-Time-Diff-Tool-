%% Copyright (c) 2011, Gianfranco Alongi (gianfranco.alongi@gmail.com)
%% All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions are met:
%%     * Redistributions of source code must retain the above copyright
%%       notice, this list of conditions and the following disclaimer.
%%     * Redistributions in binary form must reproduce the above copyright
%%       notice, this list of conditions and the following disclaimer in the
%%       documentation and/or other materials provided with the distribution.
%%     * Neither the name of Gianfranco Alongi nor the
%%       names of its contributors may be used to endorse or promote products
%%       derived from this software without specific prior written permission.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
%% WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%% DISCLAIMED. IN NO EVENT SHALL Gianfranco Alongi BE LIABLE FOR ANY
%% DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
%% (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
%% ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
%% SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-module(frame_axiom_tests).
-include_lib("eunit/include/eunit.hrl").

%% process interface
%% ---------------------------------------------------------
process_creation_diff_test() ->
    Options = [creation],
    Ref = frame_axiom:snapshot([{process,Options}]),
    Pid = synchronoulsy_start_a_process(),
    ?assertEqual([{created,Pid}],frame_axiom:diff(Ref,[{process,Options}])).

process_death_diff_test() ->
    Options = [death],
    process_flag(trap_exit,true),
    Pid = synchronoulsy_start_a_process(),
    Ref = frame_axiom:snapshot([{process,Options}]),
    synchronoulsy_kill_process(Pid),
    ?assertEqual([{died,Pid}],frame_axiom:diff(Ref,[{process,Options}])).

process_mailbox_received_test() ->
    Options = [received_messages],
    Pid = synchronoulsy_start_a_process(),
    Ref = frame_axiom:snapshot([{process,Options}]),
    Message = {iMessage,make_ref()},
    Pid ! Message,
    ?assertEqual([{received,Pid,[Message]}],
		 frame_axiom:diff(Ref,[{process,Options}])),
    synchronoulsy_kill_process(Pid).

process_consumed_messages_test() ->
    Options = [consumed_messages],
    Pid = synchronoulsy_start_a_synchronous_consumer_process(),
    Message = {iMessage,make_ref()},
    Pid ! Message,
    Ref = frame_axiom:snapshot([{process,Options}]),
    Pid ! consume,
    receive
	{Pid,consumed,Message} -> ok
    end,
    ?assertEqual([{consumed,Pid,[Message]}],
		 frame_axiom:diff(Ref,[{process,Options}])),
    synchronoulsy_kill_process(Pid).


named_process_creation_diff_test() ->
    Options = [creation_named],
    Ref = frame_axiom:snapshot([{process,Options}]),
    Pid = synchronoulsy_start_named(named_process_a),
    ?assertEqual([{created,named_process_a}],frame_axiom:diff(Ref,[{process,Options}])),
    synchronoulsy_kill_process(Pid).

named_process_dying_diff_test() ->
    Options = [death_named],
    Pid = synchronoulsy_start_named(named_process_b),
    Ref = frame_axiom:snapshot([{process,Options}]),
    synchronoulsy_kill_process(Pid),
    ?assertEqual([{died,named_process_b}],frame_axiom:diff(Ref,[{process,Options}])).
    
named_process_replaced_diff_test() ->
    Options = [replaced_named],
    Pid = synchronoulsy_start_named(named_process_c),
    Ref = frame_axiom:snapshot([{process,Options}]), 
    synchronoulsy_kill_process(Pid),
    Pid2 = synchronoulsy_start_named(named_process_c),
    ?assertEqual([{replaced,named_process_c}],frame_axiom:diff(Ref,[{process,Options}])),
    synchronoulsy_kill_process(Pid2).

all_no_change_diff_test() ->
    Options = all,
    Ref = frame_axiom:snapshot([{process,Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{process,Options}])).

%% application
%%----------------------------------------------------------
application_creation_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    application:start(snmp),
    ?assertEqual([{started,snmp}],frame_axiom:diff(Ref,application,start_stop)),
    application:stop(snmp).

application_stop_diff_test() ->
    application:start(snmp),
    Ref = frame_axiom:snapshot(application),
    application:stop(snmp),
    ?assertEqual([{stopped,snmp}],frame_axiom:diff(Ref,application,start_stop)).

application_no_change_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    ?assertEqual([],frame_axiom:diff(Ref,application,start_stop)).

application_load_diff_test() ->
    application:unload(snmp),
    Ref = frame_axiom:snapshot(application),
    application:load(snmp),
    ?assertEqual([{loaded,snmp}],frame_axiom:diff(Ref,application,load_unload)).

application_unload_diff_test() ->
    application:load(snmp),
    Ref = frame_axiom:snapshot(application),
    application:unload(snmp),
    ?assertEqual([{unloaded,snmp}],frame_axiom:diff(Ref,application,load_unload)).

application_load_no_change_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    ?assertEqual([],frame_axiom:diff(Ref,application,load_unload)).

application_load_and_start_diff_test() ->
    Ref = frame_axiom:snapshot(application),
    application:load(inets),
    application:start(snmp),
    ?assertEqual([{started,snmp},{loaded,inets},{loaded,snmp}],
		 frame_axiom:diff(Ref,application,[start_stop,
						   load_unload])),
    application:stop(snmp),
    application:unload(inets).

application_unload_and_stop_diff_test() ->
    application:load(inets),
    application:start(snmp),
    Ref = frame_axiom:snapshot(application),
    application:stop(snmp),
    application:unload(inets),
    ?assertEqual([{stopped,snmp},{unloaded,inets}],
		 frame_axiom:diff(Ref,application,[start_stop,
						   load_unload])),
    application:unload(snmp).

%% ets
%% ---------------------------------------------------------
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

%% port
%% ---------------------------------------------------------
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

%% file system
%% ---------------------------------------------------------
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

%% file system, detailed tests (size, contents)
%% ----------------------------------------------------------
file_contents_changed_test() ->
    Path = ".",
    Name = "this_file.txt",    
    FilePath = filename:join(Path,Name),
    file:write_file(Name,"Hello_World"),
    Ref = frame_axiom:snapshot({dir_detailed,Path}),
    file:write_file(Name,"Wello_Horld"),
    ?assertEqual([{content_changed,FilePath}],frame_axiom:diff(Ref,{dir_detailed,Path})).

%% node 
%% ---------------------------------------------------------
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


%% multiple type snapshot
%% ---------------------------------------------------------
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
				       ets
				      ])),
    application:stop(snmp),
    ets:delete(Ets),
    synchronoulsy_kill_process(Pid).

multiple_type_deletion_test() ->
    Pid = spawn_link(fun() -> receive _ -> ok end end),
    application:start(snmp),
    Ets = ets:new(created,[]),
    Ref = frame_axiom:snapshot([process,application,ets]),
    application:stop(snmp),
    ets:delete(Ets),
    synchronoulsy_kill_process(Pid),
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
    synchronoulsy_kill_process(Pid).

multiple_type_no_change_test() ->
    Ref = frame_axiom:snapshot([process,ets]),
    ?assertEqual([{process,[]},{ets,[]}],frame_axiom:diff(Ref,[process,ets])).

%% multiple calls to snapshot
%% ---------------------------------------------------------
second_snapshot_call_test() ->
    Ref = frame_axiom:snapshot(ets),
    Ets = ets:new(test,[]),
    frame_axiom:snapshot(Ref,application),
    application:start(snmp),
    ?assertEqual([{ets,[{created,Ets}]},
		  {application,[{started,snmp}]}
		 ],
		 frame_axiom:diff(Ref,[ets,{application,[start_stop]}])).
    

successive_snapshots_of_same_resets_test() ->    
    Ref = frame_axiom:snapshot(ets),
    Ets = ets:new(a,[]),
    frame_axiom:snapshot(Ref,ets),
    ?assertEqual([],frame_axiom:diff(Ref,ets)),
    ets:delete(Ets).


%% helpers
%% -----------------------------------------------------------------------------
synchronoulsy_start_named(Name) ->
    SharedSecret = make_ref(),
    Master = self(),
    Pid = spawn_link(fun() ->
			     register(Name,self()),
			     Master ! SharedSecret,
			     receive _ -> ok end
		     end),				      
    receive 
	SharedSecret ->
	    ok
    end,
    Pid.

synchronoulsy_start_a_process() ->
    SharedSecret = make_ref(),
    Master = self(),
    Pid = spawn_link(fun() ->
			     Master ! SharedSecret,
			     receive die -> ok end
		     end),				      
    receive 
	SharedSecret ->
	    ok
    end,
    Pid.
    
synchronoulsy_start_a_synchronous_consumer_process() ->
    SharedSecret = make_ref(),
    Master = self(),
    F = fun(F) ->
		Master ! SharedSecret,
		receive 
		    die -> ok;
		    consume ->
			receive 
			    X -> 
				Master ! {self(),consumed,X}, 
				F(F)
			end
		end
	end,
    Pid = spawn_link(fun() ->F(F)end), 
    receive 
	SharedSecret ->
	    ok
    end,
    Pid.
    

synchronoulsy_kill_process(Pid) ->
    Pid ! die,
    receive 
	{'EXIT',Pid,normal} ->
	    ok
    end.

ensure_empd() ->
    EPMD = os:cmd("epmd -names"),
    case {re:run(EPMD,"epmd: Cannot connect to local epmd.*"),
	  re:run(EPMD,"epmd: up and running on port .*")} of
	{{match,_},nomatch} ->
	    os:cmd("epmd -daemon");
	{nomatch,{match,_}} -> ok
    end.
