-module(bpe_task).
-author('Maxim Sokhatsky').
-compile(export_all).
-include("bpe.hrl").

find_flow(noflow) -> [];
find_flow([H|_]=List) when is_list(H) -> H;
find_flow([H|_]=List) when is_integer(H) -> List.
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
    case Module:action({request,Source,Target},Proc) of
         {{reply,Message},Task,State} -> {reply,{{complete,Message},Task},State};
         {reply,Task,State}           -> {reply,{complete,Task},State};
         {reply,State}                -> {reply,{complete,Target},State};
         {error,Message,Task,State}   -> {reply,{error,Message,Task},State};
         {stop,Proc}                  -> {stop,{normal,Target},Proc}
    end.

handle_task(#beginEvent{},CurrentTask,Target,Proc) ->
    task_action(Proc#process.module,CurrentTask,Target,Proc);

handle_task(#userTask{},CurrentTask,Target,Proc=#process{module=Module}) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#receiveTask{reader=_Reader},CurrentTask,Target,#process{module=Module}=Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#sendTask{writer=_Writer},CurrentTask,Target,#process{module=Module}=Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#serviceTask{},CurrentTask,Target,#process{module=Module}=Proc) ->
    task_action(Module,CurrentTask,Target,Proc);

handle_task(#gateway{type=parallel},Src,Dst,#process{module=Module}=Proc) ->
    task_action(Module,Src,Dst,Proc);

handle_task(#endEvent{},CurrentTask,Target,#process{module=Module}=Proc) ->
    task_action(Module,CurrentTask,Target,Proc),
    {stop,{normal,Target},Proc};

handle_task(_,_,Target,Proc) ->
    {reply,{unknown_task,Target},Proc}.
