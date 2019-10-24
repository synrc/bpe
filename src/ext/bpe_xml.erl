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

def() -> load("priv/sample.bpmn").

load(File) -> load(File, ?MODULE).

load(File,Module) ->
  {ok,Bin} = file:read_file(File),
  _Y = {#xmlElement{name=N,content=C}=_X,_} = xmerl_scan:string(binary_to_list(Bin)),
  _E = {'bpmn:definitions',[{'bpmn:process',Elements,Attrs}],_} = {N,find(C,'bpmn:process'),attr(C)},
  Name = proplists:get_value(id,Attrs),
  Proc = reduce(Elements,#process{name=Name},Module),
  Tasks = fillInOut(Proc#process.tasks, Proc#process.flows),
  Tasks1 = fixRoles(Tasks, Proc#process.roles),
  Proc#process{ id=[],
                tasks = Tasks1,
                roles=[],
                events = [ #boundaryEvent{name='*', timeout=#timeout{spec={0,{0,30,0}}}}
                         | Proc#process.events ] }.

reduce([], Acc, _Module) ->
  Acc;

reduce([{'bpmn:task',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#task{module=Module,name=Name}|Tasks]}, Module);

reduce([{'bpmn:startEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#beginEvent{module=Module,name=Name}|Tasks],beginEvent=Name}, Module);

reduce([{'bpmn:endEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#endEvent{module=Module,name=Name}|Tasks],endEvent=Name}, Module);

reduce([{'bpmn:sequenceFlow',Body,Attrs}|T],#process{flows=Flows} = Process, Module) ->
  Name   = proplists:get_value(id,Attrs),
  Source = proplists:get_value(sourceRef,Attrs),
  Target = proplists:get_value(targetRef,Attrs),
  F = #sequenceFlow{name=Name,source=Source,target=Target},
  reduce(T,Process#process{flows=[Flow|Flows]}, Module);

reduce([{'bpmn:conditionExpression',Body,_Attrs}|T],#sequenceFlow{} = Flow, Module) ->
  Cond = list_to_atom(hd(Body)),
  reduce(T,Flow#sequenceFlow{condition=Cond},Module);

reduce([{'bpmn:parallelGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=parallel}|Tasks]}, Module);

reduce([{'bpmn:exclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=exclusive}|Tasks]}, Module);

reduce([{'bpmn:inclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=inclusive}|Tasks]}, Module);

reduce([{'bpmn:complexGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=complex}|Tasks]}, Module);

reduce([{'bpmn:gateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process, Module) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=Module,name=Name,type=none}|Tasks]}, Module);

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
fillInOut(Tasks, [#sequenceFlow{name=Name,source=Source,target=Target}|Flows]) ->
  Tasks1 = key_push_value(Name, #gateway.out, Source, #gateway.name, Tasks),
  Tasks2 = key_push_value(Name, #gateway.in,  Target, #gateway.name, Tasks1),
  fillInOut(Tasks2, Flows). 

key_push_value(Value, ValueKey, ElemId, ElemIdKey, List) ->
  Elem = keyfind(ElemId, ElemIdKey, List),
  RecName = hd(tuple_to_list(Elem)),
  if 
    RecName == beginEvent -> List;
    RecName == endEvent -> List;
    true ->
      NewElem = setelement(ValueKey, Elem, [Value|element(ValueKey,Elem)]),
      keyreplace(ElemId, ElemIdKey, List, NewElem)
  end.

fixRoles(Tasks, []) -> Tasks;
fixRoles(Tasks, [Lane|Lanes]) ->
  LaneAttributes = element(3,Lane),
  RoleName = proplists:get_value(name,LaneAttributes),
  Role = list_to_atom(RoleName),
  TaskIdsToUpdateRoles = [T || {'bpmn:flowNodeRef',[],{value,T}} <- element(2,Lane)],
  fixRoles(update_roles(TaskIdsToUpdateRoles, Tasks, Role), Lanes).

update_roles([], AllTasks, _Role) -> AllTasks;
update_roles([TaskId|Rest], AllTasks, Role) ->
  update_roles(Rest,key_push_value(Role, #task.roles, TaskId, #task.name, AllTasks),Role).

action({request,_,_},P) -> {reply,P}.

auth(_) -> true.
