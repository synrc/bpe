-module(bpe_task).

-author('Maxim Sokhatsky').

-export([find_flow/1,
         find_flow/2,
         move_doclink/2,
         targets/2,
         denied_flow/2,
         already_finished/1,
         task_action/4,
         handle_task/4]).

-include("bpe.hrl").

find_flow(noflow) -> [];
find_flow([H | _] = _List) when is_list(H) -> H;
find_flow([H | _] = List) when is_integer(H) -> List.

find_flow([], List) -> find_flow(List);
find_flow(Stage, List) ->
    case lists:member(Stage, List) of
        true -> Stage;
        _ -> find_flow(List)
    end.

move_doclink({_Source, _Target}, _Proc) -> [].

targets(Name, Proc) ->
    lists:flatten([Target
                   || #sequenceFlow{source = Source, target = Target}
                          <- Proc#process.flows,
                      Source == Name]).

denied_flow(Curr, Proc) ->
    {reply, {denied_flow, Curr}, Proc}.

already_finished(Proc) -> {stop, {normal, []}, Proc}.

task_action(Module, Source, Target, Proc) ->
    {M, F, A} = proplists:get_value(flow_callback,
                                    Proc#process.etc,
                                    {bpe_task, move_doclink, [{Source, Target}, Proc]}),
    case Module:action({request, Source, Target}, Proc) of
        {{reply, Message}, Task, State} ->
            apply(M, F, A),
            {reply, {{complete, Message}, Task}, State};
        {reply, Task, State} ->
            apply(M, F, A),
            {reply, {complete, Task}, State};
        {reply, State} ->
            apply(M, F, A),
            {reply, {complete, Target}, State};
        {error, Message, Task, State} ->
            {reply, {error, Message, Task}, State};
        {stop, Proc} -> {stop, {normal, Target}, Proc}
    end.

handle_task(#beginEvent{}, CurrentTask, Target, Proc) ->
    task_action(Proc#process.module,
                CurrentTask,
                Target,
                Proc);
handle_task(#userTask{}, CurrentTask, Target,
            Proc = #process{module = Module}) ->
    task_action(Module, CurrentTask, Target, Proc);
handle_task(#receiveTask{reader = _Reader}, CurrentTask,
            Target, #process{module = Module} = Proc) ->
    task_action(Module, CurrentTask, Target, Proc);
handle_task(#sendTask{writer = _Writer}, CurrentTask,
            Target, #process{module = Module} = Proc) ->
    task_action(Module, CurrentTask, Target, Proc);
handle_task(#serviceTask{}, CurrentTask, Target,
            #process{module = Module} = Proc) ->
    task_action(Module, CurrentTask, Target, Proc);
handle_task(#gateway{type = parallel}, Src, Dst,
            #process{module = Module} = Proc) ->
    task_action(Module, Src, Dst, Proc);
handle_task(#endEvent{}, CurrentTask, Target,
            #process{module = Module} = Proc) ->
    task_action(Module, CurrentTask, Target, Proc),
    {stop, {normal, Target}, Proc};
handle_task(_, _, Target, Proc) ->
    {reply, {unknown_task, Target}, Proc}.
