-module(bpe_gate).
-compile(export_all).
-include("bpe.hrl").

gate_action(Module,_Name,Event,Target,Proc) ->
    case Module:action({gateway,Event#gateway.name},Proc) of
         {reply,State}      -> {reply,{complete,Target},State};
         {reply,Task,State} -> {reply,{complete,Task},State} end.

handle_event(#gateway{type=parallel},Target,Proc) -> 
    {reply,{complete,Target},Proc};

handle_event(#gateway{type=exclusive},Target,Proc) -> 
    {reply,{complete,Target},Proc};

handle_event(#gateway{type=inclusive},Target,Proc) -> 
    {reply,{complete,Target},Proc};

handle_event(#gateway{type=complex},Target,Proc) -> 
    {reply,{complete,Target},Proc};

handle_event(_,Target,Proc) ->
    {reply,{unknown_event,Target},Proc}.
