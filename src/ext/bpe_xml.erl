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
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    Proc = reduce(Elements,#process{id=Id,name=Name,module=Module}),
    Tasks = fillInOut(Proc#process.tasks, Proc#process.flows),
    Tasks1 = fixRoles(Tasks, Proc#process.roles),
    Proc#process{ id=[],
                  tasks = Tasks1,
                  xml = filename:basename(File, ".bpmn"),
                  events = [ #boundaryEvent{id='*', name="All", timeout=#timeout{spec={0,{0,30,0}}}}
                           | Proc#process.events ] }.

reduce([], Acc) ->
    Acc;

reduce([{'bpmn:task',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#task{id=Id,name=Name}|Tasks]});

reduce([{'bpmn:startEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#beginEvent{id=Id,name=Name}|Tasks],beginEvent=Id});

reduce([{'bpmn:endEvent',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#endEvent{id=Id,name=Name}|Tasks],endEvent=Id});

reduce([{'bpmn:sequenceFlow',Body,Attrs}|T],#process{flows=Flows} = Process) ->
    Id   = proplists:get_value(id,Attrs),
    Name   = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    Source = proplists:get_value(sourceRef,Attrs),
    Target = proplists:get_value(targetRef,Attrs),
    F = #sequenceFlow{id=Id,name=Name,source=Source,target=Target},
    Flow = reduce(Body,F),
    reduce(T,Process#process{flows=[Flow|Flows]});

reduce([{'bpmn:conditionExpression',Body,_Attrs}|T],#sequenceFlow{} = Flow) ->
    {ok, Ts, _} = erl_scan:string(hd(Body)),
    {ok, Cond} = erl_parse:parse_term(Ts),
    reduce(T,Flow#sequenceFlow{condition=Cond});

reduce([{'bpmn:parallelGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{id=Id,name=Name,type=parallel}|Tasks]});

reduce([{'bpmn:exclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{id=Id,name=Name,type=exclusive}|Tasks]});

reduce([{'bpmn:inclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{id=Id,name=Name,type=inclusive}|Tasks]});

reduce([{'bpmn:complexGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{id=Id,name=Name,type=complex}|Tasks]});

reduce([{'bpmn:gateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{id=Id,name=Name}|Tasks]});

reduce([{'bpmn:laneSet',Lanes,Attrs}|T], Process) ->
    Roles = [ #role{ id = proplists:get_value(id,Att,[]),
                     tasks = [ Name || {_, [], {value, Name}} <- Tasks ],
                     name = unicode:characters_to_binary(proplists:get_value(name,Att,[]),utf16)
                   } || {_,Tasks,Att} <- Lanes],
    io:format("Lanes: ~p~n",[Lanes]),
    io:format("Roles: ~p~n",[Roles]),
    reduce(T,Process#process{roles = Roles});

%%TODO? Maybe add support for those intries and remove them from this guard
reduce([{SkipType,_Body,_Attrs}|T],#process{} = Process)
    when SkipType == 'bpmn:dataObjectReference';
         SkipType == 'bpmn:dataObject';
         SkipType == 'bpmn:association';
         SkipType == 'bpmn:textAnnotation' ->
    skip,
    reduce(T,Process).

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
fixRoles(Tasks, [#role{id=Id,name=Name,tasks=XmlTasks}|Lanes]) -> fixRoles(update_roles(XmlTasks, Tasks, Id), Lanes).

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
