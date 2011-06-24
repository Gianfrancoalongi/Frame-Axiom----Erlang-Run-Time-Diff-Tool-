%%% @author Gianfranco <zenon@zen.home>
%%% @copyright (C) 2011, Gianfranco
%%% Created : 24 Jun 2011 by Gianfranco <zenon@zen.home>
-module(frame_axiom).
-export([snapshot/1]).
-export([diff/2,
	 diff/3]).

snapshot(process) ->
    Ets = ets:new(process,[private]),
    Processes = erlang:processes(),
    ets:insert(Ets,{process,Processes}),
    Ets;
snapshot(application) ->
    Ets = ets:new(application,[private]),
    Running = application:which_applications(),
    ets:insert(Ets,{application,Running}),
    Ets.    

diff(Ets,process) ->
    Processes = erlang:processes(),
    [{process,Recorded}] = ets:lookup(Ets,process),
    Dead = [{died,P} || P <- Recorded, not lists:member(P,Processes)],
    Created = [{created,P} || P <- Processes, not lists:member(P,Recorded)],
    Dead++Created.
    
diff(Ets,application,[start_stop]) ->    
    Running = application:which_applications(),
    [{application,Recorded}] = ets:lookup(Ets,application),
    Started = [{started,hd(tuple_to_list(App))} ||App <- Running, not lists:member(App,Recorded)],
    Stopped = [{stopped,hd(tuple_to_list(App))} ||App <- Recorded, not lists:member(App,Running)],
    Started++Stopped.
