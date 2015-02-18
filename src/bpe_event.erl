-module(bpe_event).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").

event_action(Module,Name,Event,Target,Proc) ->
    Allowed = case erlang:function_exported(Module, action, 2) of
                   true ->
    case Module:action({event,Event#messageEvent.name,Event#messageEvent.payload},Proc) of
         {next,State}       -> bpe_proc:process_flow([],Proc,false);
         {reply,State}      -> {reply,{complete,Target},State};
         {reply,Task,State} -> {reply,{complete,Task},State} end;
                   false -> case wf:config(bpe,ignore_exports) of
                               [] -> {reply,{complete,Target},Proc};
                                _ -> {reply,{module_not_exported,Target},Proc} end end.

handle_event(#beginEvent{},Target,Proc) -> 
    {reply,{complete,Target},Proc};

handle_event(#endEvent{},Target,Proc) ->
    {stop,{normal,Target},Proc};

handle_event(#messageEvent{module=Module,name=Name}=Event,Target,Proc) ->
    event_action(Module,Name,Event,Target,Proc);

handle_event(_,Target,Proc) ->
    {reply,{unknown_event,Target},Proc}.
