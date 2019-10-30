-module(bpe_xml).
-include_lib("bpe/include/bpe.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).
-import(lists,[keyfind/3, keyreplace/4]).

attr(E) -> [ {N,V} || #xmlAttribute{name=N,value=V} <- E].

find(E=[#xmlText{}, #xmlElement{name='bpmn:conditionExpression'} | _],[]) ->
  [{X,[V],attr(A)} || #xmlElement{name=X,attributes=A,content=[#xmlText{value=V}]} <- E];
%%It is expected that bpmn:flowNodeRef are present only in bpmn:lane and only #xmlText{} and #xmlElement{name='bpmn:flowNodeRef'} are present there 
find(E=[#xmlText{}, #xmlElement{name='bpmn:flowNodeRef'} | _],[]) ->
  [{X,[],{value,V}} || #xmlElement{name=X,content=[#xmlText{value=V}]} <- E];
find(E,[]) -> [{X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E];
find(E, I) -> [{X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E, X == I].


nextTask(Acc, Flows, Task, EndEvent) when Task == EndEvent orelse Flows == [] -> Acc;
nextTask(Acc, Flows, Task, EndEvent) -> 
  % io:format("acc before ~p task=~p~n", [Acc,Task]),
  {F, Other} = lists:partition(fun(F0) -> F0#sequenceFlow.source == Task end, Flows),
  IsTask = lists:any(fun(T) -> T==Task end, Acc), 
  AccT = case IsTask of
          true -> Acc;
          false -> [Task|Acc]
        end,
  % io:format("next acc ~p~n", [AccT]),
  lists:foldl(fun(F0, Acc0) -> nextTask(Acc0, Other, F0#sequenceFlow.target, EndEvent) end, AccT, F)
.
def() -> load("priv/sample.bpmn").

load(File) -> load(File, ?MODULE).

load(File,Module) ->
    {ok,Bin} = file:read_file(File),
    _Y = {#xmlElement{name=N,content=C}=_X,_} = xmerl_scan:string(binary_to_list(Bin)),
    _E = {'bpmn:definitions',[{'bpmn:process',Elements,Attrs}],_} = {N,find(C,'bpmn:process'),attr(C)},
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    Proc = reduce(Elements,#process{id=Id,name=Name},Module),

    BeginEvent = Proc#process.beginEvent,
    EndEvent = Proc#process.endEvent,
    Flows = Proc#process.flows,
    Tasks = Proc#process.tasks,
    SortTasks0 = nextTask([BeginEvent], Flows, BeginEvent, EndEvent),
    SortTasks = lists:reverse(lists:map(fun(ST) -> 
                                            lists:keyfind(ST, 2, Tasks)
                                          end, [EndEvent | SortTasks0])),
  
    Tasks0 = fillInOut(SortTasks, Flows),
    Tasks1 = fixRoles(Tasks0, Proc#process.roles),
    Proc#process{ id=[],
                  tasks = Tasks1,
                  roles=[],
                  xml = filename:basename(File, ".bpmn"),
                  events = [ #boundaryEvent{id='*', name="All", timeout=#timeout{spec={0,{0,30,0}}}}
                           | Proc#process.events ] }.

reduce([], Acc, _Module) ->
    Acc;

reduce([{'bpmn:task',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#task{module=Module,id=Id,name=Name}|Tasks]}, Module);

reduce([{'bpmn:startEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#beginEvent{module=Module,id=Id,name=Name}|Tasks],beginEvent=Id}, Module);

reduce([{'bpmn:endEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#endEvent{module=Module,id=Id,name=Name}|Tasks],endEvent=Id}, Module);

reduce([{'bpmn:sequenceFlow',Body,Attrs}|T],#process{flows=Flows} = Process, Module) ->
    Id   = proplists:get_value(id,Attrs),
    Name   = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    Source = proplists:get_value(sourceRef,Attrs),
    Target = proplists:get_value(targetRef,Attrs),
    F = #sequenceFlow{id=Id,name=Name,source=Source,target=Target},
    Flow = reduce(Body,F,Module),
    reduce(T,Process#process{flows=[Flow|Flows]}, Module);

reduce([{'bpmn:conditionExpression',Body,_Attrs}|T],#sequenceFlow{} = Flow, Module) ->
    {ok, Ts, _} = erl_scan:string(hd(Body)),
    {ok, Cond} = erl_parse:parse_term(Ts),
    reduce(T,Flow#sequenceFlow{condition=Cond},Module);

reduce([{'bpmn:parallelGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{module=Module,id=Id,name=Name,type=parallel}|Tasks]}, Module);

reduce([{'bpmn:exclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{module=Module,id=Id,name=Name,type=exclusive}|Tasks]}, Module);

reduce([{'bpmn:inclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{module=Module,id=Id,name=Name,type=inclusive}|Tasks]}, Module);

reduce([{'bpmn:complexGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{module=Module,id=Id,name=Name,type=complex}|Tasks]}, Module);

reduce([{'bpmn:gateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{module=Module,id=Id,name=Name,type=none}|Tasks]}, Module);

reduce([{'bpmn:laneSet',Lanes,_Attrs}|T], Process, Module) ->
    reduce(T,Process#process{roles = Lanes}, Module);

%%TODO? Maybe add support for those intries and remove them from this guard
reduce([{SkipType,_Body,_Attrs}|T],#process{} = Process, Module)
    when SkipType == 'bpmn:dataObjectReference';
         SkipType == 'bpmn:dataObject';
         SkipType == 'bpmn:association';
         SkipType == 'bpmn:textAnnotation' ->
    skip,
    reduce(T,Process, Module).

%%TODO?: Maybe use incoming/outgoing from XML itself instead of fillInOut
fillInOut(Tasks, []) -> Tasks;
fillInOut(Tasks, [#sequenceFlow{id=Name,source=Source,target=Target}|Flows]) ->
    Tasks1 = key_push_value(Name, #gateway.out, Source, #gateway.id, Tasks),
    Tasks2 = key_push_value(Name, #gateway.in,  Target, #gateway.id, Tasks1),
    fillInOut(Tasks2, Flows).

key_push_value(Value, ValueKey, ElemId, ElemIdKey, List) ->
    Elem = keyfind(ElemId, ElemIdKey, List),
    RecName = hd(tuple_to_list(Elem)),
    case RecName of
         endEvent   -> List;
                  _ -> keyreplace(ElemId, ElemIdKey, List,
                       setelement(ValueKey, Elem, [Value|element(ValueKey,Elem)])) end.

fixRoles(Tasks, []) -> Tasks;
fixRoles(Tasks, [Lane|Lanes]) ->
    LaneAttributes = element(3,Lane),
    RoleId = proplists:get_value(id,LaneAttributes),
    Role = proplists:get_value(name,LaneAttributes),
    TaskIdsToUpdateRoles = [T || {'bpmn:flowNodeRef',[],{value,T}} <- element(2,Lane)],
    fixRoles(update_roles(TaskIdsToUpdateRoles, Tasks, Role), Lanes).

update_roles([], AllTasks, _Role) -> AllTasks;
update_roles([TaskId|Rest], AllTasks, Role) ->
    update_roles(Rest,key_push_value(Role, #task.roles, TaskId, #task.id, AllTasks),Role).

action({request,_,_},P) -> {reply,P}.

auth(_) -> true.

% support bindings and evaluation for @umka1332
% > :bpe_xml.parse("{1,2,[],{[1],'2',erlang:get(1)}}.")
% {1, 2, [], {[1], :"2", :undefined}}

parse(Term) when is_binary(Term) ->
  parse(binary_to_list(Term));

parse(Term) ->
  {ok,Tokens,_} = erl_scan:string(Term),
  {ok,AbsForm}  = erl_parse:parse_exprs(Tokens),
  {_,Value,_}   = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  Value.
