%%% @author Gianfranco <zenon@zen.home>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 24 Jun 2011 by Gianfranco <zenon@zen.home>
-module(frame_axiom).
-export([snapshot/1]).
-export([diff/2,
	 diff/3]).
-export([collect/1]).

snapshot(process) ->
    Ets = ets:new(process,[private]),
    Processes = erlang:processes(),
    ets:insert(Ets,{process,Processes}),
    Ets;
snapshot(application) ->
    Ets = ets:new(application,[private]),
    Running = application:which_applications(),
    Loaded = application:loaded_applications(),
    ets:insert(Ets,{application,Running,Loaded}),
    Ets;
snapshot(ets) ->
    Ets = ets:new(ets,[private]),
    Existing = ets:all(),
    ets:insert(Ets,{ets,Existing}),
    Ets;
snapshot(port) ->
    Ets = ets:new(ets,[private]),
    Existing = erlang:ports(),
    ets:insert(Ets,{port,Existing}),
    Ets;
snapshot({dir,Path}) ->
    Ets = ets:new(ets,[private]),
    Structure = collect(Path),
    ets:insert(Ets,{{dir,Path},Structure}),
    Ets.


diff(Ets,process) ->
    Processes = erlang:processes(),
    [{process,Recorded}] = ets:lookup(Ets,process),
    Dead = [{died,P} || P <- Recorded, not lists:member(P,Processes)],
    Created = [{created,P} || P <- Processes, not lists:member(P,Recorded)],
    Dead++Created;
diff(Ets,ets) ->
    Current = ets:all(),
    [{ets,Recorded}] = ets:lookup(Ets,ets),
    Created = [{created,E}||E<-Current,not lists:member(E,Recorded)],
    Deleted = [{deleted,E}||E<-Recorded,not lists:member(E,Current)],
    Created++Deleted;
diff(Ets,port) ->
    Ports = erlang:ports(),
    [{port,Recorded}] = ets:lookup(Ets,port),
    Opened = [{opened,P}||P<-Ports,not lists:member(P,Recorded)], 
    Closed = [{closed,P}||P<-Recorded,not lists:member(P,Ports)], 
    Opened++Closed;
diff(Ets,{dir,Path}) ->
    Current = collect(Path),
    [{{dir,Path},Recorded}] = ets:lookup(Ets,{dir,Path}),
    Created = [{created,S} || S <- Current, not lists:member(S,Recorded)],
    Created.

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
