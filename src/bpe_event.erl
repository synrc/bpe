-module(bpe_event).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").

event_action(Module,Name,Event,Curr,Target,Proc) ->
    case Module:action({event,Name,Event#messageEvent.payload}, Proc) of
         {reply,State}       -> {reply,{complete,Target},State};
         {reply,Task,State} -> {reply,{complete,Task},State} end.

handle_event(#beginEvent{},_Curr,Target,Proc) ->
    {reply,{complete,Target},Proc};

handle_event(#endEvent{},_Curr,Target,Proc) ->
    {stop,{normal,Target},Proc};

handle_event(#messageBeginEvent{},_Curr,Target,Proc) ->
    {stop,{normal,Target},Proc};

handle_event(#messageEvent{name=Name}=Event,Curr,Target,#process{module=Module}=Proc) ->
    event_action(Module,Name,Event,Curr,Target,Proc);

handle_event(_,_Curr,Target,Proc) ->
    {reply,{unknown_event,Target},Proc}.
