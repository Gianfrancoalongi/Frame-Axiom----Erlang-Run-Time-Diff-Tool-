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

snapshot({D,Path}) when D == dir_detailed ;
			D == dir ->
    snapshot(ets:new(snapshot,[private]),{D,Path});
snapshot(SnapShot) when SnapShot == application;
			SnapShot == ets;
			SnapShot == port;
			SnapShot == node;
			SnapShot == named_process ->
    snapshot(ets:new(snapshot,[private]),SnapShot);
snapshot(SnapShots) when is_list(SnapShots) ->
    lists:foldl(fun(SnapShot,Ets) -> snapshot(Ets,SnapShot)
		end,ets:new(snapshot,[private]),SnapShots).

snapshot(Ets,{process,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,process,Option) 
		end,Ets,Options);
snapshot(Ets,{process,all}) ->
    snapshot(Ets,{process,all(process)});
snapshot(Ets,{application,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,EtsAcc) -> snapshot(EtsAcc,application,Option) 
		end,Ets,Options);

snapshot(Ets,process) ->
    Processes = erlang:processes(),
    ets:insert(Ets,{process,Processes}),
    Ets;
snapshot(Ets,application) ->
    Running = application:which_applications(),
    Loaded = application:loaded_applications(),
    ets:insert(Ets,{application,Running,Loaded}),
    Ets;
snapshot(Ets,ets) ->
    Existing = ets:all(),
    ets:insert(Ets,{ets,Existing}),
    Ets;
snapshot(Ets,port) ->
    Existing = erlang:ports(),
    ets:insert(Ets,{port,Existing}),
    Ets;
snapshot(Ets,{dir,Path}) ->
    Structure = collect(false,Path),
    ets:insert(Ets,{{dir,Path},Structure}),
    Ets;
snapshot(Ets,{dir_detailed,Path}) ->
    Structure = collect(true,Path),
    ets:insert(Ets,{{dir_detailed,Path},Structure}),
    Ets;
snapshot(Ets,node) ->
    Current = nodes(),
    ets:insert(Ets,{node,Current}),
    Ets.

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
    Ets.
    

diff(Ets,[X]) -> 
    diff(Ets,X);
diff(Ets,DiffSpecs) when is_list(DiffSpecs) ->
    lists:foldl(fun(DiffSpec,Res) -> 
			Key = diffspec_key(DiffSpec),
			Res++[{Key,diff(Ets,DiffSpec)}]
		end,[],DiffSpecs);
diff(Ets,{process,all}) ->
    diff(Ets,{process,all(process)});
diff(Ets,{process,Options}) when is_list(Options) ->
    lists:foldl(fun(Option,Res) -> Res++diff(Ets,process,Option) 
		end,[],Options);
diff(Ets,process) ->
    Processes = erlang:processes(),
    [{process,Recorded}] = ets:lookup(Ets,process),
    {Dead,Created} = split(created,died,Processes,Recorded),
    Dead++Created;
diff(Ets,ets) ->
    Current = ets:all(),
    [{ets,Recorded}] = ets:lookup(Ets,ets),
    {Created,Deleted} = split(created,deleted,Current,Recorded),
    Created++Deleted;
diff(Ets,port) ->
    Ports = erlang:ports(),
    [{port,Recorded}] = ets:lookup(Ets,port),
    {Opened,Closed} = split(opened,closed,Ports,Recorded),
    Opened++Closed;
diff(Ets,{dir,Path}) ->
    Current = collect(false,Path),
    [{{dir,Path},Recorded}] = ets:lookup(Ets,{dir,Path}),
    {Created,Deleted} = split(created,deleted,Current,Recorded),
    Created++Deleted;
diff(Ets,{dir_detailed,Path}) ->
    Current = collect(true,Path),
    [{{dir_detailed,Path},Recorded}] = ets:lookup(Ets,{dir_detailed,Path}),
    Changed = contents_changed(Recorded,Current),
    Changed;
diff(Ets,{application,DiffSpec}) ->
    diff(Ets,application,DiffSpec);
diff(Ets,node) ->
    Current = nodes(),
    [{node,Recorded}] = ets:lookup(Ets,node),
    {Connected,Disconnected} = split(connected,disconnected,Current,Recorded),
    Connected++Disconnected.

diff(Ets,application,Modes) when is_list(Modes) ->
    lists:foldl(fun(Mode,Acc) -> Acc++diff(Ets,application,Mode) end,[],Modes);

diff(Ets,application,start_stop) -> 
    Running = application:which_applications(),
    [{application,Recorded,_}] = ets:lookup(Ets,application),
    Started = [{started,hd(tuple_to_list(App))} ||App <- Running, not lists:member(App,Recorded)],
    Stopped = [{stopped,hd(tuple_to_list(App))} ||App <- Recorded, not lists:member(App,Running)],
    Started++Stopped;
diff(Ets,application,load_unload) ->
    Loaded = application:loaded_applications(),
    [{application,_,Recorded}] = ets:lookup(Ets,application),
    NewLoaded = [{loaded,hd(tuple_to_list(App))} ||App <- Loaded, not lists:member(App,Recorded)], 
    UnLoaded = [{unloaded,hd(tuple_to_list(App))} ||App <- Recorded, not lists:member(App,Loaded)],
    NewLoaded++UnLoaded;
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
    [{stopped,S}||S<-Recorded,not lists:member(S,Running)].


	      


%% Helpers section
%% ----------------------------------------------------------
all(process) ->
    [creation,death,received_messages,consumed_messages,
     creation_named,death_named,replaced_named].

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

diffspec_key({application,_}) ->
    application;
diffspec_key(X) -> X.

split(KeyA,KeyB,As,Bs) ->
    {[{KeyA,A}||A<-As,not lists:member(A,Bs)],
     [{KeyB,B}||B<-Bs,not lists:member(B,As)]}.

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
