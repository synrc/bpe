-module(bpe_event).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").

event_action(Module,_Name,Event,Target,Proc) ->
    case Module:action({event,Event#messageEvent.name,Event#messageEvent.payload},Proc) of
         {run,State}        -> bpe_proc:run('Finish',State);
         {until,Task,State} -> bpe_proc:run(Task,State);
         {reply,State}      -> {reply,{complete,Target},State};
         {reply,Task,State} -> {reply,{complete,Task},State} end.

handle_event(#beginEvent{},Target,Proc) -> 
    {reply,{complete,Target},Proc};

handle_event(#endEvent{},Target,Proc) ->
    {stop,{normal,Target},Proc};

handle_event(#messageEvent{module=Module,name=Name}=Event,Target,Proc) ->
    event_action(Module,Name,Event,Target,Proc);

handle_event(_,Target,Proc) ->
    {reply,{unknown_event,Target},Proc}.
