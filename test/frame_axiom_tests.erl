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
		 frame_axiom:diff(Ref,[{process,Options}])).

process_consumed_messages_test() ->
    timer:sleep(300), %% Let the Eunit processes settle in peace
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
		 frame_axiom:diff(Ref,[{process,Options}])).


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
    timer:sleep(300), %% Let the Eunit processes settle in peace    
    Options = all,
    Ref = frame_axiom:snapshot([{process,Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{process,Options}])).

all_change_diff_test() ->
    timer:sleep(300), %% Let the Eunit processes settle in peace    
    process_flag(trap_exit,true),
    Options = all,
    Killed = synchronoulsy_start_a_process(),
    Received = synchronoulsy_start_a_process(),
    Consumed = synchronoulsy_start_a_synchronous_consumer_process(),
    ConsumedMessage = {cMessage,make_ref()},
    ReceivedMessage = {rMessage,make_ref()},
    Consumed ! ConsumedMessage,
    Replaced = synchronoulsy_start_named(named_process_c),
    DiedNamed = synchronoulsy_start_named(named_process_b),
    Ref = frame_axiom:snapshot([{process,Options}]),
    Started = synchronoulsy_start_a_process(),
    Received ! ReceivedMessage,
    synchronoulsy_kill_process(Killed),
    synchronoulsy_kill_process(Replaced),
    Replacer = synchronoulsy_start_named(named_process_c),
    NamedCreated = synchronoulsy_start_named(named_process_a),
    Consumed ! consume,
    receive
    	{Consumed,consumed,ConsumedMessage} -> ok
    end,    
    synchronoulsy_kill_process(DiedNamed),
    ?assertEqual([{created,Started},
		  {created,Replacer},
		  {created,NamedCreated},
		  {died,Killed},
		  {died,Replaced},
		  {died,DiedNamed},
		  {received,Received,[ReceivedMessage]},
		  {consumed,Consumed,[ConsumedMessage]},
		  {created,named_process_a},
		  {died,named_process_b},
		  {replaced,named_process_c}
		 ],
		 frame_axiom:diff(Ref,[{process,Options}])),
    synchronoulsy_kill_process(NamedCreated),
    synchronoulsy_kill_process(Replacer).


%% application
%%----------------------------------------------------------
application_creation_diff_test() ->
    Options = [started],
    Ref = frame_axiom:snapshot([{application,Options}]),
    application:start(snmp),
    ?assertEqual([{started,snmp}],frame_axiom:diff(Ref,[{application,Options}])),
    application:stop(snmp).

application_stop_diff_test() ->
    Options = [stopped],
    application:start(snmp),
    Ref = frame_axiom:snapshot([{application,Options}]),
    application:stop(snmp),
    ?assertEqual([{stopped,snmp}],frame_axiom:diff(Ref,[{application,Options}])).

application_load_diff_test() ->
    Options = [loaded],
    application:unload(snmp),
    Ref = frame_axiom:snapshot([{application,Options}]),
    application:load(snmp),
    ?assertEqual([{loaded,snmp}],frame_axiom:diff(Ref,[{application,Options}])).

application_unload_diff_test() ->
    Options = [unloaded],
    application:load(snmp),
    Ref = frame_axiom:snapshot([{application,Options}]),
    application:unload(snmp),
    ?assertEqual([{unloaded,snmp}],frame_axiom:diff(Ref,[{application,Options}])).

application_all_diff_test() ->
    Options = all,
    application:start(odbc),
    application:load(inets),
    Ref = frame_axiom:snapshot([{application,Options}]),
    application:unload(inets),
    application:start(snmp),
    application:stop(odbc),
    ?assertEqual([{loaded,snmp},
		  {unloaded,inets},
		  {started,snmp},
		  {stopped,odbc}
		 ],
		 frame_axiom:diff(Ref,[{application,Options}])),
    application:stop(snmp),
    application:unload(snmp),
    application:unload(inets).

application_all_no_diff_test() ->
    Options = all,
    Ref = frame_axiom:snapshot([{application,Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{application,Options}])).

%% ets
%% ---------------------------------------------------------
ets_creation_diff_test() ->
    Options = [creation],
    Ref = frame_axiom:snapshot([{ets,Options}]),
    New = ets:new(some,[]),
    ?assertEqual([{created,New}],frame_axiom:diff(Ref,[{ets,Options}])).

ets_delection_diff_test() ->
    Options = [deletion],
    E = ets:new(some,[]),
    Ref = frame_axiom:snapshot([{ets,Options}]),
    ets:delete(E),
    ?assertEqual([{deleted,E}],frame_axiom:diff(Ref,[{ets,Options}])).

ets_all_no_change_test() ->
    Options = all,
    Ref = frame_axiom:snapshot([{ets,Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{ets,Options}])).
        
ets_all_change_test() ->
    Options = all,
    D = ets:new(some,[]),
    Ref = frame_axiom:snapshot([{ets,Options}]),
    C = ets:new(other,[]),
    ets:delete(D),
    ?assertEqual([{created,C},
		  {deleted,D}],
		 frame_axiom:diff(Ref,[{ets,Options}])).

%% port
%% ---------------------------------------------------------
ports_creation_diff_test() ->
    Options = [opened],
    Ref = frame_axiom:snapshot([{port,Options}]),
    P = erlang:open_port({spawn,"cd"},[stream]),
    ?assertEqual([{opened,P}],frame_axiom:diff(Ref,[{port,Options}])),
    erlang:port_close(P).

ports_closing_diff_test() ->
    Options = [closed],
    P = erlang:open_port({spawn,"cd"},[stream]),    
    Ref = frame_axiom:snapshot([{port,Options}]),
    erlang:port_close(P),
    ?assertEqual([{closed,P}],frame_axiom:diff(Ref,[{port,Options}])).

ports_all_no_change_diff_test() ->
    Options = all,
    Ref = frame_axiom:snapshot([{port,Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{port,Options}])).

%% file system
%% ---------------------------------------------------------
file_direct_under_creation_diff_test() ->
    Path = ".",
    Options = [{creation,Path}],
    Ref = frame_axiom:snapshot([{dir,Options}]),
    Name= "created_this.txt",
    file:write_file(Name,"HelloWorld"),
    FilePath = filename:join(Path,Name),
    ?assertEqual([{created,{file,FilePath}}],frame_axiom:diff(Ref,[{dir,Options}])),
    file:delete(FilePath).

file_direct_under_deletion_diff_test() ->
    Path = ".",
    Options = [{deletion,Path}],
    Name= "created_this.txt",
    file:write_file(Name,"HelloWorld"),
    Ref = frame_axiom:snapshot([{dir,Options}]),
    FilePath = filename:join(Path,Name),
    file:delete(FilePath),
    ?assertEqual([{deleted,{file,FilePath}}],frame_axiom:diff(Ref,[{dir,Options}])).

file_directory_creation_test() ->
    Path = ".",
    Options = [{creation,Path}],
    Name= "this_dir",
    Ref = frame_axiom:snapshot([{dir,Options}]),
    FullPath = filename:join(Path,Name),
    ok = filelib:ensure_dir(FullPath++"/"),
    ?assertEqual([{created,{dir,FullPath}}],frame_axiom:diff(Ref,[{dir,Options}])),
    file:del_dir(FullPath).

file_directory_deletion_test() ->
    Path = ".",
    Options = [{deletion,Path}],
    Name= "this_dir",
    FullPath = filename:join(Path,Name),
    ok = filelib:ensure_dir(FullPath++"/"),
    Ref = frame_axiom:snapshot([{dir,Options}]),
    file:del_dir(FullPath),    
    ?assertEqual([{deleted,{dir,FullPath}}],frame_axiom:diff(Ref,[{dir,Options}])).

file_contents_changed_test() ->
    Path = ".",
    Options = [{content_changes,Path}],
    Name = "this_file.txt",
    FilePath = filename:join(Path,Name),
    file:write_file(Name,"Hello_World"),
    Ref = frame_axiom:snapshot([{dir,Options}]),
    file:write_file(Name,"Wello_Horld"),
    ?assertEqual([{content_changed,FilePath}],frame_axiom:diff(Ref,[{dir,Options}])).

file_no_change_all_diff_test() ->
    Path = ".",
    Options = {all,Path},
    Ref = frame_axiom:snapshot([{dir,Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{dir,Options}])).


%% node 
%% ---------------------------------------------------------
node_connected_diff_test() ->
    ensure_empd(),
    net_kernel:start([box_box,shortnames]),
    Options = [connection],
    Ref = frame_axiom:snapshot([{node,Options}]),
    {ok,Node} = slave:start(list_to_atom(inet_db:gethostname()),'slave'),
    ?assertEqual([{connected,Node}],frame_axiom:diff(Ref,[{node,Options}])),
    slave:stop(Node),
    net_kernel:stop().

node_disconnected_diff_test() ->
    ensure_empd(),
    net_kernel:start([box_box,shortnames]),
    Options = [disconnection],
    {ok,Node} = slave:start(list_to_atom(inet_db:gethostname()),'slave'),
    Ref = frame_axiom:snapshot([{node,Options}]),
    slave:stop(Node),
    ?assertEqual([{disconnected,Node}],frame_axiom:diff(Ref,[{node,Options}])),
    net_kernel:stop().

node_no_diff_all_test() ->
    Options = all,
    Ref = frame_axiom:snapshot([{node,Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{node,Options}])).

node_all_diff_test() ->
    ensure_empd(),
    net_kernel:start([box_box,shortnames]),
    Options = all,
    {ok,Node1} = slave:start(list_to_atom(inet_db:gethostname()),'slave_one'),
    Ref = frame_axiom:snapshot([{node,Options}]),
    slave:stop(Node1),
    {ok,Node2} = slave:start(list_to_atom(inet_db:gethostname()),'slave_two'),
    ?assertEqual([{connected,Node2},
		  {disconnected,Node1}],frame_axiom:diff(Ref,[{node,Options}])),
    slave:stop(Node2),
    net_kernel:stop().    
    
%% multiple type snapshot
%% ---------------------------------------------------------
multiple_type_creation_test() ->
    P_Options = [creation],
    A_Options = [started],
    E_Options = [creation],
    Ref = frame_axiom:snapshot([{process,P_Options},
				{application,A_Options},
				{ets,E_Options}]),
    Pid = synchronoulsy_start_a_process(),
    application:start(snmp),
    Ets = ets:new(created,[]),
    ?assertMatch([
		  {process,[{created,Pid},_,_,_]},
		  {application,[{started,snmp}]},
		  {ets,[{created,Ets}]}
		 ],
		 frame_axiom:diff(Ref,[{process,P_Options},
				       {application,A_Options},
				       {ets,E_Options}
				      ])),
    application:stop(snmp),
    ets:delete(Ets),
    synchronoulsy_kill_process(Pid).

multiple_type_deletion_test() ->
    P_Options = [death],
    A_Options = [stopped],
    E_Options = [deletion],
    Pid = synchronoulsy_start_a_process(),
    application:start(snmp),
    Ets = ets:new(created,[]),
    Ref = frame_axiom:snapshot([{process,P_Options},
				{application,A_Options},
				{ets,E_Options}]),
    application:stop(snmp),
    ets:delete(Ets),
    synchronoulsy_kill_process(Pid),
    ?assertMatch([
		  {process,[{died,Pid},_,_,_]},
		  {application,[{stopped,snmp}]},
		  {ets,[{deleted,Ets}]}
		 ],
		 frame_axiom:diff(Ref,[{process,P_Options},
				       {application,A_Options},
				       {ets,E_Options}])).
    
multiple_type_mixed_test() ->
    P_Options = [creation],
    E_Options = [deletion],
    Ets = ets:new(created,[]),
    Ref = frame_axiom:snapshot([{process,P_Options},
				{ets,E_Options}]),
    Pid = synchronoulsy_start_a_process(),
    ets:delete(Ets),
    ?assertMatch([
		  {process,[{created,Pid}]},
		  {ets,[{deleted,Ets}]}
		 ],
		 frame_axiom:diff(Ref,[{process,P_Options},
				       {ets,E_Options}])),
    synchronoulsy_kill_process(Pid).

multiple_type_no_change_test() ->    
    Ref = frame_axiom:snapshot([{application,all},
				{ets,all}]),
    ?assertEqual([{application,[]},{ets,[]}],
		 frame_axiom:diff(Ref,[{application,all},
				       {ets,all}])).

%% multiple calls to snapshot
%% ---------------------------------------------------------
second_snapshot_call_test() ->
    E_Options = [creation],
    A_Options = [started],
    Ref = frame_axiom:snapshot([{ets,E_Options}]),
    Ets = ets:new(test,[]),
    frame_axiom:snapshot(Ref,[{application,A_Options}]),
    application:start(snmp),
    ?assertEqual([{ets,[{created,Ets}]},
    		  {application,[{started,snmp}]}
    		 ],
    		 frame_axiom:diff(Ref,[{ets,E_Options},
    				       {application,A_Options}])),
    application:stop(snmp).
    
successive_snapshots_of_same_resets_test() ->    
    E_Options = all,
    Ref = frame_axiom:snapshot([{ets,E_Options}]),
    Ets = ets:new(a,[]),
    frame_axiom:snapshot(Ref,[{ets,E_Options}]),
    ?assertEqual([],frame_axiom:diff(Ref,[{ets,E_Options}])),
    ets:delete(Ets).

%% helpers
%% -----------------------------------------------------------------------------
synchronoulsy_start_named(Name) ->
    SharedSecret = make_ref(),
    Master = self(),
    Pid = spawn_link(fun() ->
			     register(Name,self()),
			     Master ! SharedSecret,
			     receive die -> ok end
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
    R = fun() -> Master ! SharedSecret end,
    F = fun(F,T) ->
		T(),
		receive 
		    die -> ok;
		    consume ->
			receive 
			    X -> 
				Master ! {self(),consumed,X}, 
				F(F,fun() -> ok end)
			end
		end
	end,
    Pid = spawn_link(fun() ->F(F,R) end),
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
