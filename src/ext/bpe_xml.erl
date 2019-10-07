-module(bpe_xml).
-include_lib("bpe/include/bpe.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).
-import(lists,[keyfind/3, keyreplace/4]).

attr(E) -> [ {N,V} || #xmlAttribute{name=N,value=V} <- E].

find(E,[]) -> [ {X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E];
find(E, I) -> [ {X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E, X == I].

load() ->
  {ok,Bin} = file:read_file("priv/diagram_1.bpmn"),
  Y = {#xmlElement{name=N,content=C}=X,_} = xmerl_scan:string(binary_to_list(Bin)),
  E = {'bpmn:definitions',[{'bpmn:process',Elements,Attrs}],_} = {N,find(C,'bpmn:process'),attr(C)},
  io:format("DEBUG: ~p~n",[E]),
  Proc = reduce(Elements,#process{}),
  Proc#process{tasks = fillInOut(Proc#process.tasks, Proc#process.flows)}.

reduce([],Acc) ->
  Acc;

reduce([{'bpmn:task',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#task{name=Name}|Tasks]});

reduce([{'bpmn:startEvent',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#beginEvent{name=Name}|Tasks],beginEvent=Name});

reduce([{'bpmn:endEvent',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#endEvent{name=Name}|Tasks],endEvent=Name});

reduce([{'bpmn:sequenceFlow',Body,Attrs}|T],#process{flows=Flows} = Process) ->
  Name   = proplists:get_value(id,Attrs),
  Source = proplists:get_value(sourceRef,Attrs),
  Target = proplists:get_value(targetRef,Attrs),
  reduce(T,Process#process{flows=[#sequenceFlow{name=Name,source=Source,target=Target}|Flows]});

reduce([{'bpmn:parallelGateway',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{name=Name,type=parallel}|Tasks]});

reduce([{'bpmn:exclusiveGateway',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{name=Name,type=exclusive}|Tasks]});

reduce([{'bpmn:inclusiveGateway',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{name=Name,type=inclusive}|Tasks]});

reduce([{'bpmn:complexGateway',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{name=Name,type=complex}|Tasks]});

reduce([{'bpmn:gateway',Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
  Name = proplists:get_value(id,Attrs),
  reduce(T,Process#process{tasks=[#gateway{name=Name,type=none}|Tasks]}).

fillInOut(Tasks, []) -> Tasks;
fillInOut(Tasks, [#sequenceFlow{name=Name,source=Source,target=Target}|Flows]) ->
  Tasks1 = case keyfind(Source, #gateway.name, Tasks) of
             false -> [#gateway{name=Source,type=parallel,outputs=[Name]}];%%should be error
             T = #gateway{outpus=L} -> keyreplace(Source,#gateway.name,Tasks,T#gateway{outputs=[Name|L]})
           end,
  Tasks2 = case keyfind(Target, #gateway.name, Tasks) of
             false -> [#gateway{name=Target,type=parallel,inputs=[Name]}];%%should be error
             T1 = #gateway{inputs=L1} -> keyreplace(Target,#gateway.name,Tasks1,T1#gateway{inputs=[Name|L1]})
           end,
  fillInOut(Tasks2, Flows). 
