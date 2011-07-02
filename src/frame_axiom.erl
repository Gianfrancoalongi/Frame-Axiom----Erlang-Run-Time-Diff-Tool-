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
-module(frame_axiom).
-export([snapshot/1,
	 snapshot/2]).
-export([diff/2,
	 diff/3]).

snapshot(SnapShots) when is_list(SnapShots) ->
    lists:foldl(fun(SnapShot,Ets) -> snapshot(Ets,SnapShot)
		end,ets:new(snapshot,[private]),SnapShots).

snapshot(Ets,SnapSpecs) when is_list(SnapSpecs) ->
    lists:foldl(fun(SnapSpec,Res) -> snapshot(Res,SnapSpec) end,
	       Ets,SnapSpecs);
snapshot(Ets,{process,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,process,Option) 
		end,Ets,Options);
snapshot(Ets,{process,all}) ->
    snapshot(Ets,{process,all(process)});
snapshot(Ets,{application,all}) ->
    snapshot(Ets,{application,all(application)});
snapshot(Ets,{application,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,application,Option) 
		end,Ets,Options);
snapshot(Ets,{ets,all}) ->
    snapshot(Ets,{ets,all(ets)});
snapshot(Ets,{ets,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,ets,Option) 
		end,Ets,Options);
snapshot(Ets,{port,all}) ->
    snapshot(Ets,{port,all(port)});
snapshot(Ets,{port,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,port,Option) 
		end,Ets,Options);
snapshot(Ets,{dir,{all,Path}}) ->
    snapshot(Ets,{dir,[{X,Path}||X<-all(dir)]});
snapshot(Ets,{dir,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,dir,Option)
		end,Ets,Options);
snapshot(Ets,{node,all}) ->
    snapshot(Ets,{node,all(node)});
snapshot(Ets,{node,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,node,Option)
		end,Ets,Options).
    

snapshot(Ets,process,creation) ->    
    Pids = erlang:processes(),
    ets:insert(Ets,{{process,creation},Pids}),
    Ets;
snapshot(Ets,process,death) -> 
    Pids = erlang:processes(),
    ets:insert(Ets,{{process,death},Pids}),
    Ets;
snapshot(Ets,process,creation_named) -> 
    Current = named_processes(),
    ets:insert(Ets,{{process,creation_named},Current}),
    Ets;
snapshot(Ets,process,death_named) -> 
    Current = named_processes(),
    ets:insert(Ets,{{process,creation_death},Current}),
    Ets;
snapshot(Ets,process,replaced_named) -> 
    Current = named_processes_with_pid(),
    ets:insert(Ets,{{process,replaced_named},Current}),
    Ets;
snapshot(Ets,process,received_messages) -> 
    MessagesWithPids = messages_with_pids(),
    ets:insert(Ets,{{process,received_messages},MessagesWithPids}),
    Ets;
snapshot(Ets,process,consumed_messages) -> 
    MessagesWithPids = messages_with_pids(),
    ets:insert(Ets,{{process,consumed_messages},MessagesWithPids}),
    Ets;
snapshot(Ets,application,started) -> 
    Running = [element(1,A)||A<-application:which_applications()],
    ets:insert(Ets,{{application,started},Running}),
    Ets;
snapshot(Ets,application,stopped) -> 
    Running = [element(1,A)||A<-application:which_applications()],
    ets:insert(Ets,{{application,stopped},Running}),
    Ets;
snapshot(Ets,application,loaded) -> 
    Loaded = [element(1,A)||A<-application:loaded_applications()],
    ets:insert(Ets,{{application,loaded},Loaded}),
    Ets;
snapshot(Ets,application,unloaded) -> 
    Loaded = [element(1,A)||A<-application:loaded_applications()],
    ets:insert(Ets,{{application,unloaded},Loaded}),
    Ets;
snapshot(Ets,ets,creation) -> 
    Existing = ets:all(),
    ets:insert(Ets,{{ets,creation},Existing}),
    Ets;
snapshot(Ets,ets,deletion) -> 
    Existing = ets:all(),
    ets:insert(Ets,{{ets,deletion},Existing}),
    Ets;
snapshot(Ets,port,opened) -> 
    Ports = erlang:ports(),
    ets:insert(Ets,{{port,opened},Ports}),
    Ets;
snapshot(Ets,port,closed) -> 
    Ports = erlang:ports(),
    ets:insert(Ets,{{port,closed},Ports}),
    Ets;
snapshot(Ets,dir,{creation,Path}) -> 
    Current = collect(false,Path),
    ets:insert(Ets,{{creation,Path},Current}),
    Ets;
snapshot(Ets,dir,{deletion,Path}) -> 
    Current = collect(false,Path),
    ets:insert(Ets,{{deletion,Path},Current}),
    Ets;
snapshot(Ets,dir,{content_changes,Path}) -> 
    Current = collect(true,Path),
    ets:insert(Ets,{{content_changes,Path},Current}),
    Ets;
snapshot(Ets,node,connection) -> 
    Current = nodes(),
    ets:insert(Ets,{{node,connection},Current}),
    Ets;
snapshot(Ets,node,disconnection) -> 
    Current = nodes(),
    ets:insert(Ets,{{node,disconnection},Current}),
    Ets.

     
diff(Ets,[X]) -> 
    diff(Ets,X);
diff(Ets,DiffSpecs) when is_list(DiffSpecs) ->
    lists:foldl(fun({DiffType,Options},Res) ->
			Res++[{DiffType,diff(Ets,{DiffType,Options})}]
		end,[],DiffSpecs);
diff(Ets,{process,all}) ->
    diff(Ets,{process,all(process)});
diff(Ets,{process,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,Res) -> Res++diff(Ets,process,Option) 
		end,[],Options);
diff(Ets,{application,all}) ->
    diff(Ets,{application,all(application)});
diff(Ets,{application,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,Res) -> Res++diff(Ets,application,Option) 
		end,[],Options);    
diff(Ets,{ets,all}) ->
    diff(Ets,{ets,all(ets)});
diff(Ets,{ets,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,Res) -> Res++diff(Ets,ets,Option) 
		end,[],Options);
diff(Ets,{port,all}) ->
    diff(Ets,{port,all(port)});
diff(Ets,{port,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,Res) -> Res++diff(Ets,port,Option) 
		end,[],Options);
diff(Ets,{dir,{all,Path}}) ->
    diff(Ets,{dir,[{X,Path}||X<-all(dir)]});
diff(Ets,{dir,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,Res) -> Res++diff(Ets,dir,Option) 
		end,[],Options);
diff(Ets,{node,all}) ->
    diff(Ets,{node,all(node)});
diff(Ets,{node,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,Res) -> Res++diff(Ets,node,Option) 
		end,[],Options).

diff(Ets,process,creation) ->
    CurrentPids = erlang:processes(),
    Key = {process,creation},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{created,P}||P<-CurrentPids,not lists:member(P,Recorded)];
diff(Ets,process,death) ->
    CurrentPids = erlang:processes(),
    Key = {process,death},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{died,P}||P<-Recorded,not lists:member(P,CurrentPids)];
diff(Ets,process,creation_named) ->
    CurrentNamed = named_processes(),
    Key = {process,creation_named},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{created,P}||P<-CurrentNamed,not lists:member(P,Recorded)];
diff(Ets,process,death_named) ->
    CurrentNamed = named_processes(),
    Key = {process,creation_death},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{died,P}||P<-Recorded,not lists:member(P,CurrentNamed)];
diff(Ets,process,replaced_named) ->
    CurrentNamedWithPid = named_processes_with_pid(),
    Key = {process,replaced_named},
    [{Key,RecordedWithPid}] = ets:lookup(Ets,Key),
    [{replaced,N}||{P,N} <- CurrentNamedWithPid,
		   proplists:get_value(P,RecordedWithPid) == undefined andalso
		       lists:keyfind(N,2,RecordedWithPid) =/= false];
diff(Ets,process,received_messages) ->
    MessagesWithPids = messages_with_pids(),
    Key = {process,received_messages},
    [{Key,RecordedWithPid}] = ets:lookup(Ets,Key),
    lists:foldl(
      fun({RecPid,RecMessages},Acc) ->
	      OldMessages = proplists:get_value(RecPid,MessagesWithPids),
	      case OldMessages of
		  undefined -> Acc;
		  _ ->
		      case [R||R<-OldMessages,not lists:member(R,RecMessages)] of
			  [] -> Acc;
			  X -> Acc++[{received,RecPid,X}]
		      end
	      end
      end,[],RecordedWithPid);
diff(Ets,process,consumed_messages) ->
    MessagesWithPids = messages_with_pids(),
    Key = {process,consumed_messages},
    [{Key,RecordedWithPid}] = ets:lookup(Ets,Key),
    lists:foldl(
      fun({RecPid,RecMessages},Acc) ->
	      OldMessages = proplists:get_value(RecPid,MessagesWithPids),
	      case [R||R<-RecMessages,not lists:member(R,OldMessages)] of
		  [] -> Acc;
		  X -> Acc++[{consumed,RecPid,X}]
	      end
      end,[],RecordedWithPid);
diff(Ets,application,started) ->
    Running = [element(1,A)||A<-application:which_applications()],
    Key = {application,started},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{started,S}||S<-Running,not lists:member(S,Recorded)];
diff(Ets,application,stopped) ->
    Running = [element(1,A)||A<-application:which_applications()],
    Key = {application,stopped},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{stopped,S}||S<-Recorded,not lists:member(S,Running)];
diff(Ets,application,loaded) ->
    Loaded = [element(1,A)||A<-application:loaded_applications()],
    Key = {application,loaded},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{loaded,L}||L<-Loaded,not lists:member(L,Recorded)];
diff(Ets,application,unloaded) ->
    Loaded = [element(1,A)||A<-application:loaded_applications()],
    Key = {application,unloaded},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{unloaded,U}||U<-Recorded,not lists:member(U,Loaded)];
diff(Ets,ets,creation) ->
    Current = ets:all(),
    Key = {ets,creation},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{created,E}||E<-Current,not lists:member(E,Recorded)];
diff(Ets,ets,deletion) ->
    Current = ets:all(),
    Key = {ets,deletion},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{deleted,E}||E<-Recorded,not lists:member(E,Current)];
diff(Ets,port,opened) ->
    Current = erlang:ports(),
    Key = {port,opened},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{opened,P}||P<-Current,not lists:member(P,Recorded)];
diff(Ets,port,closed) ->
    Current = erlang:ports(),
    Key = {port,closed},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{closed,C}||C<-Recorded,not lists:member(C,Current)];
diff(Ets,dir,{creation,Path}) ->
    Current = collect(false,Path),
    Key = {creation,Path},    
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{created,C}||C<-Current,not lists:member(C,Recorded)];
diff(Ets,dir,{deletion,Path}) ->
    Current = collect(false,Path),
    Key = {deletion,Path},    
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{deleted,C}||C<-Recorded,not lists:member(C,Current)];
diff(Ets,dir,{content_changes,Path}) ->
    Current = collect(true,Path),
    Key = {content_changes,Path},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    contents_changed(Recorded,Current);
diff(Ets,node,connection) ->
    Current = nodes(),
    Key = {node,connection},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{connected,N}||N<-Current,not lists:member(N,Recorded)];
diff(Ets,node,disconnection) ->
    Current = nodes(),
    Key = {node,disconnection},
    [{Key,Recorded}] = ets:lookup(Ets,Key),
    [{disconnected,N}||N<-Recorded,not lists:member(N,Current)].
    
%% Helpers section
%% ----------------------------------------------------------
all(process) ->
    [creation,death,received_messages,consumed_messages,
     creation_named,death_named,replaced_named];
all(application) ->
    [loaded,unloaded,started,stopped];
all(ets) ->
    [creation,deletion];
all(port) ->
    [opened,closed];
all(node) ->
    [connection,disconnection];
all(dir) ->
    [creation,deletion,content_changes].

collect(ExactP,Path) ->
    case filelib:is_dir(Path) of
	true ->
	    {ok,Files} = file:list_dir(Path),
	    Head = case ExactP of
		       true -> {dir,Path,0};
		       false ->{dir,Path}
		   end,
	    [Head|lists:foldl(fun(F,Acc)->collect(ExactP,F)++Acc end,[],
			      [filename:join(Path,File)||File<-Files])];
	false ->
	    case ExactP of
		true ->
		    {ok,Bin} = file:read_file(Path),
		    MD5 = crypto:md5(Bin),
		    [{file,Path,MD5}];
		false ->
		    [{file,Path}]
	    end
    end.

named_processes() ->
    Procs = [{P,erlang:process_info(P,registered_name)}||P<-erlang:processes()],
    lists:foldl(fun({_,{registered_name,N}},Acc) -> Acc++[N];
		   (_,Acc) -> Acc 
		end,[],Procs).

named_processes_with_pid() ->    
    Procs = [{P,erlang:process_info(P,registered_name)}||P<-erlang:processes()],
    lists:foldl(fun({P,{registered_name,N}},Acc) -> Acc++[{P,N}];
    		   (_,Acc) -> Acc 
    		end,[],Procs).

contents_changed([{file,Path,MD5}|R],Current) ->
    case lists:keyfind(Path,2,Current) of
	{file,Path,MD5} ->
	    contents_changed(R,Current);
	{file,Path,_} ->
	    [{content_changed,Path}|contents_changed(R,Current)]
    end;
contents_changed([_|R],Current) -> contents_changed(R,Current);
contents_changed([],_) -> [].

messages_with_pids() ->
    Processes = erlang:processes(),
    [{P,element(2,erlang:process_info(P,messages))}||P<-Processes].
