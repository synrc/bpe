-module(bpe_task).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").

find_flow(List) -> [H|_] = List, H.
find_flow([],List) -> find_flow(List);
find_flow(Stage,List) -> case lists:member(Stage,List) of
                              true -> Stage;
                              _ -> find_flow(List) end.

targets(Name,Proc) ->
    lists:flatten([ Target || #sequenceFlow{source=Source,target=Target}
                           <- Proc#process.flows,  Source==Name]).

denied_flow(Curr,Proc) ->
    {reply,{denied_flow,Curr},Proc}.

already_finished(Proc) ->
    {stop,{normal,[]},Proc}.

task_action(Module,Source,Target,Proc) ->
  try
    case Module:action({request,Source,Target},Proc) of
         {{reply,Message},Task,State} -> {reply,{{complete,Message},Task},State};
         {reply,Task,State}           -> {reply,{complete,Task},State};
         {reply,State}                -> {reply,{complete,Target},State};
         {error,Message,Task,State}   -> {reply,{error,Message,Task},State};
         {stop,Proc}                  -> {stop,{normal,Target},Proc}
    end
  catch X:{A,B}:Z ->
    {reply,{error,A,Target},Proc}
  end.

handle_task(#beginEvent{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#userTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#receiveTask{module=Module,reader=_Reader},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#sendTask{module=Module,writer=_Writer},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#serviceTask{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#gateway{type=parallel,module=Module},Src,Dst,Proc) ->
    task_action(Module,Src,Dst,Proc);

handle_task(#endEvent{module=Module},CurrentTask,Target,Proc) ->
    task_action(Module,CurrentTask,Target,Proc),
    {stop,{normal,Target},Proc};

handle_task(_,_,Target,Proc) ->
    {reply,{unknown_task,Target},Proc}.
