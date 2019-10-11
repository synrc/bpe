-module(bpe_xml).
-include_lib("bpe/include/bpe.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).
-import(lists,[keyfind/3, keyreplace/4]).

attr(E) -> [ {N,V} || #xmlAttribute{name=N,value=V} <- E].

find(E,[]) -> [ {X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E];
find(E, I) -> [ {X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E, X == I].

def() -> load("priv/diagram_1.bpmn").

load(File) ->
  {ok,Bin} = file:read_file(File),
  _Y = {#xmlElement{name=N,content=C}=_X,_} = xmerl_scan:string(binary_to_list(Bin)),
  E = {'bpmn:definitions',[{'bpmn:process',Elements,Attrs}],_} = {N,find(C,'bpmn:process'),attr(C)},
  io:format("DEBUG: ~p~n",[E]),
  Name = proplists:get_value(id,Attrs),
  Proc = reduce(Elements,#process{id=Name}),
  Proc#process{id=kvs:seq([],[]),tasks = fillInOut(Proc#process.tasks, Proc#process.flows)}.

reduce([],Acc) ->
  Acc;

reduce([{'bpmn:task',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#task{module=?MODULE,name=Name}|Tasks]});

reduce([{'bpmn:startEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#beginEvent{module=?MODULE,name=Name}|Tasks],beginEvent=Name});

reduce([{'bpmn:endEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#endEvent{module=?MODULE,name=Name}|Tasks],endEvent=Name});

reduce([{'bpmn:sequenceFlow',_Body,Attrs}|T],#process{flows=Flows} = Process) ->
  Name   = proplists:get_value(id,Attrs),
  Source = proplists:get_value(sourceRef,Attrs),
  Target = proplists:get_value(targetRef,Attrs),
  reduce(T,Process#process{flows=[#sequenceFlow{name=Name,source=Source,target=Target}|Flows]});

reduce([{'bpmn:parallelGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=?MODULE,name=Name,type=parallel}|Tasks]});

reduce([{'bpmn:exclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=?MODULE,name=Name,type=exclusive}|Tasks]});

reduce([{'bpmn:inclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=?MODULE,name=Name,type=inclusive}|Tasks]});

reduce([{'bpmn:complexGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=?MODULE,name=Name,type=complex}|Tasks]});

reduce([{'bpmn:gateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{module=?MODULE,name=Name,type=none}|Tasks]}).

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

action({request,_,_},P) -> {reply,P}.
