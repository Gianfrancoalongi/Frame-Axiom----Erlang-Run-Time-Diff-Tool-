-module(frame_axiom).
-export([snapshot/1]).
-export([diff/2]).

snapshot(process) ->
    Ets = ets:new(process,[private]),
    Processes = erlang:processes(),
    ets:insert(Ets,{process,Processes}),
    Ets.

diff(Ets,process) ->
    Processes = erlang:processes(),
    [{process,Recorded}] = ets:lookup(Ets,process),
    Dead = [{died,P} || P <- Recorded, not lists:member(P,Processes)],
    Created = [{created,P} || P <- Processes, not lists:member(P,Recorded)],
    Dead++Created.
    

