-module(bpe_task).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").

find_flow(List) -> [H|_] = List, H.
find_flow([],List) -> find_flow(List);
find_flow(Stage,List) -> case lists:member(Stage,List) of
                              true -> Stage;
                              _ -> find_flow(List) end.

targets(Curr,Proc) ->
    Targets = [ Target || #sequenceFlow{source=Source,target=Target} <- Proc#process.flows, 
                Source==Curr].

denied_flow(Curr,Proc) ->
    {reply,{denied_flow,Curr},Proc}.

already_finished(Proc) ->
    {stop,{normal,[]},Proc}.

task_action(Module,CurrentTask,Target,Proc) ->
    Allowed = case erlang:function_exported(Module, action, 2) of
                   true ->
    case Module:action({request,CurrentTask},Proc) of
         {reply,State}      -> {reply,{complete,Target},State};
         {reply,Task,State} -> {reply,{complete,Task},State} end;
                   false -> case wf:config(bpe,ignore_exports) of
                               [] -> {reply,{complete,Target},Proc};
                                _ -> {reply,{module_not_exported,Target},Proc} end end.

handle_task(#beginEvent{},CurrentTask,Target,Proc) -> 
    {reply,{complete,Target},Proc};

handle_task(#userTask{module=Module},CurrentTask,Target,Proc) -> 
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#receiveTask{module=Module},CurrentTask,Target,Proc) -> 
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#serviceTask{module=Module},CurrentTask,Target,Proc) -> 
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#endEvent{},CurrentTask,Target,Proc) ->
    {stop,{normal,Target},Proc};

handle_task(_,_,Target,Proc) ->
    {reply,{unknown_task,Target},Proc}.
