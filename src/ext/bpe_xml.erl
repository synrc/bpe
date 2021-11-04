-module(bpe_xml).
-include_lib("bpe/include/bpe.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-export([ns/2, def/0, attr/1, find/2, load/1, load/2, reduce/2, fillInOut/2, fixRoles/2, update_roles/3, parse/1, auth/1, action/2]).
-import(lists,[keyfind/3, keyreplace/4]).

-define(MODEL, 'http://www.omg.org/spec/BPMN/20100524/MODEL').

ns(#xmlElement{name=N, nsinfo=[], namespace=#xmlNamespace{default=?MODEL}}=E,S) ->
  N1 = list_to_atom("bpmn:"++atom_to_list(N)),
  {E#xmlElement{name=N1, nsinfo={"bpmn", N}}, S};
ns(#xmlElement{name=N, nsinfo=[], namespace=#xmlNamespace{default=[], nodes=Nds}}=E,S) ->
  case lists:keyfind(?MODEL, 2, Nds) of
    {P,_} -> 
      N1 = list_to_atom(P++":"++atom_to_list(N)),
      {E#xmlElement{name=N1, nsinfo={P,N}},S};
    false -> {E,S} end;
ns(E,S) -> {E,S}.

attr(E) -> [ {N,V} || #xmlAttribute{name=N,value=V} <- E].

find(E=[#xmlText{}, #xmlElement{name='bpmn:conditionExpression'} | _],[]) ->
  [{X,[V],attr(A)} || #xmlElement{name=X,attributes=A,content=[#xmlText{value=V}]} <- E];
find(E=[#xmlText{}, #xmlElement{name='bpmn:flowNodeRef'} | _],[]) ->
  [{X,[],{value,V}} || #xmlElement{name=X,content=[#xmlText{value=V}]} <- E];
find(E,[]) -> [{X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E];
find(E, I) -> [{X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E, X == I].

def() -> load("priv/sample.bpmn").

load(File) -> load(File, ?MODULE).

load(File,Module) ->
    case xmerl_scan:file(File, [{hook_fun, fun ns/2}]) of 
      {error, R} -> {error, R};
      {#xmlElement{name=N,content=C},_} ->
        _E = {'bpmn:definitions',[{'bpmn:process',Elements,Attrs}],_} = {N,find(C,'bpmn:process'),attr(C)},
        Id = proplists:get_value(id,Attrs),
        Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
        Proc = reduce(Elements,#process{id=Id,name=Name,module=Module}),
        Tasks = fillInOut(Proc#process.tasks, Proc#process.flows),
        Tasks1 = fixRoles(Tasks, Proc#process.roles),
        Proc#process{ id=[],
                      tasks = Tasks1,
                      xml = filename:basename(File, ".bpmn"),
                      events = Proc#process.events } end.

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
    reduce(T,Flow#sequenceFlow{condition = parse(hd(Body))});

reduce([{'bpmn:parallelGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{id=Id,name=Name,type=parallel}|Tasks]});

reduce([{'bpmn:exclusiveGateway',_Body,Attrs}|T],#process{tasks=Tasks} = Process) ->
    Id = proplists:get_value(id,Attrs),
    Default = proplists:get_value(default,Attrs,[]),
    Name = unicode:characters_to_binary(proplists:get_value(name,Attrs,[])),
    reduce(T,Process#process{tasks=[#gateway{id=Id,name=Name,type=exclusive,def=Default}|Tasks]});

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

reduce([{'bpmn:laneSet',Lanes,_Attrs}|T], Process) ->
    Roles = [ #role{ id = proplists:get_value(id,Att,[]),
                     tasks = [ Name || {_, [], {value, Name}} <- Tasks ],
                     name = unicode:characters_to_binary(proplists:get_value(name,Att,[]),utf16)
                   } || {_,Tasks,Att} <- Lanes],
%   io:format("Lanes: ~p~n",[Lanes]),
%   io:format("Roles: ~p~n",[Roles]),
    reduce(T,Process#process{roles = Roles});

reduce([{SkipType,_Body,_Attrs}|T],#process{} = Process)
    when SkipType == 'bpmn:dataObjectReference';
         SkipType == 'bpmn:dataObject';
         SkipType == 'bpmn:association';
         SkipType == 'bpmn:textAnnotation';
         SkipType == 'bpmn:extensionElements' ->
    skip,
    reduce(T,Process).

fillInOut(Tasks, []) -> Tasks;
fillInOut(Tasks, [#sequenceFlow{id=Name,source=Source,target=Target}|Flows]) ->
    Tasks1 = key_push_value(Name, #gateway.output, Source, #gateway.id, Tasks),
    Tasks2 = key_push_value(Name, #gateway.input,  Target, #gateway.id, Tasks1),
    fillInOut(Tasks2, Flows).

key_push_value(Value, ValueKey, ElemId, ElemIdKey, List) ->
    Elem = keyfind(ElemId, ElemIdKey, List),
    RecName = hd(tuple_to_list(Elem)),
    case RecName of
         endEvent   -> List;
                  _ -> keyreplace(ElemId, ElemIdKey, List,
                       setelement(ValueKey, Elem, [Value|element(ValueKey,Elem)])) end.

fixRoles(Tasks, []) -> Tasks;
fixRoles(Tasks, [#role{id=Id,name=_Name,tasks=XmlTasks}|Lanes]) -> fixRoles(update_roles(XmlTasks, Tasks, Id), Lanes).

update_roles([], AllTasks, _Role) -> AllTasks;
update_roles([TaskId|Rest], AllTasks, Role) ->
    update_roles(Rest,key_push_value(Role, #task.roles, TaskId, #task.id, AllTasks),Role).

action({request,_,_},P) -> {reply,P}.

auth(_) -> true.

parse(Term) when is_binary(Term) ->
  parse(binary_to_list(Term));

parse(Term) ->
  {ok,Tokens,_} = erl_scan:string(Term),
  {ok,AbsForm}  = erl_parse:parse_exprs(Tokens),
  {_,Value,_}   = erl_eval:exprs(AbsForm, erl_eval:new_bindings()),
  Value.
