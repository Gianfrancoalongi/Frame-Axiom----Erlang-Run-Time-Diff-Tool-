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

snapshot({dir,Path}) -> 
    snapshot(ets:new(snapshot,[private]),{dir,Path});
snapshot(SnapShot) when SnapShot == process;
			SnapShot == application;
			SnapShot == ets;
			SnapShot == port;
			SnapShot == node;
			SnapShot == named_process ->
    snapshot(ets:new(snapshot,[private]),SnapShot);
snapshot(SnapShots) when is_list(SnapShots) ->
    lists:foldl(fun(SnapShot,Ets) -> snapshot(Ets,SnapShot)
		end,ets:new(snapshot,[private]),SnapShots).

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
    Structure = collect(Path),
    ets:insert(Ets,{{dir,Path},Structure}),
    Ets;
snapshot(Ets,node) ->
    Current = nodes(),
    ets:insert(Ets,{node,Current}),
    Ets;
snapshot(Ets,named_process) ->
    Current = named_processes(),
    ets:insert(Ets,{named_process,Current}),
    Ets.

diff(Ets,DiffSpecs) when is_list(DiffSpecs) ->
    lists:foldl(fun(DiffSpec,Res) -> 
			Key = diffspec_key(DiffSpec),
			Res++[{Key,diff(Ets,DiffSpec)}]
		end,[],DiffSpecs);
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
    Current = collect(Path),
    [{{dir,Path},Recorded}] = ets:lookup(Ets,{dir,Path}),
    {Created,Deleted} = split(created,deleted,Current,Recorded),
    Created++Deleted;
diff(Ets,{application,[start_stop]}) ->
    diff(Ets,application,[start_stop]);
diff(Ets,node) ->
    Current = nodes(),
    [{node,Recorded}] = ets:lookup(Ets,node),
    {Connected,Disconnected} = split(connected,disconnected,Current,Recorded),
    Connected++Disconnected;
diff(Ets,named_process) ->
    Current = named_processes(),
    [{named_process,Recorded}] = ets:lookup(Ets,named_process),
    Replaced = [{replaced,Name}||{Pid,Name}<-Current,
				 proplists:get_value(Pid,Recorded) == undefined andalso 
				     lists:keyfind(Name,2,Recorded) =/= false],
    {Created,Deleted} = split(created,died,[C||{_,C}<-Current],[R||{_,R}<-Recorded]),
    Created++Deleted++Replaced.

diff(Ets,application,[start_stop]) -> 
    Running = application:which_applications(),
    [{application,Recorded,_}] = ets:lookup(Ets,application),
    Started = [{started,hd(tuple_to_list(App))} ||App <- Running, not lists:member(App,Recorded)],
    Stopped = [{stopped,hd(tuple_to_list(App))} ||App <- Recorded, not lists:member(App,Running)],
    Started++Stopped;
diff(Ets,application,[load_unload]) ->
    Loaded = application:loaded_applications(),
    [{application,_,Recorded}] = ets:lookup(Ets,application),
    NewLoaded = [{loaded,hd(tuple_to_list(App))} ||App <- Loaded, not lists:member(App,Recorded)], 
    UnLoaded = [{unloaded,hd(tuple_to_list(App))} ||App <- Recorded, not lists:member(App,Loaded)],
    NewLoaded++UnLoaded.



collect(Path) ->
    case filelib:is_dir(Path) of
	true ->
	    {ok,Files} = file:list_dir(Path),
	    [{dir,Path}|lists:foldl(fun(F,Acc)->collect(F)++Acc end,[],
			      [filename:join(Path,File)||File<-Files])];
	false ->
	    [{file,Path}]
    end.

diffspec_key({application,_}) ->
    application;
diffspec_key(X) -> X.

split(KeyA,KeyB,As,Bs) ->
    {[{KeyA,A}||A<-As,not lists:member(A,Bs)],
     [{KeyB,B}||B<-Bs,not lists:member(B,As)]}.

named_processes() ->
    Procs = [{P,erlang:process_info(P,registered_name)}||P<-erlang:processes()],
    lists:foldl(fun({P,{registered_name,N}},Acc) -> Acc++[{P,N}];
		   (_,Acc) -> Acc 
		end,[],Procs).
