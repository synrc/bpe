-module(bpe_gateways).
-author('Oleksandr Naumov').
-include("bpe.hrl").
-include("doc.hrl").
-export([def/0]).
-compile(export_all).
-import(lists, [reverse/1, keyfind/3, keyreplace/4]).

def() ->
    R_raw=[{r1, a,b},
           {r2, b,c},
           {r3, b,d},
           {r4, c,e},
           {r5, c,f},
           {r6, d,g},
           {r7, e,h},
           {r8, h,j},
           {r9, f,i},
           {r10,i,j},
           {r11,f,g},
           {r12,g,j},
           {r13,j,k}],
    {R,N}=getEdgesAndVertices(R_raw),
    P = #process { name = 'Graph',
                   flows = R,
                   tasks = N,
                   beginEvent = a,
                   endEvent = k,
                   events=[]},
    fixGateways(fixBeginEndEvents(P)).

vertices(R) -> vertices(reverse(R),[]).
vertices([], N) -> N;
vertices([#sequenceFlow{id=Id, source=From, target=To} | Rtail], N) ->
    N1   = case keyfind(From, #task.id, N) of
                false -> [#task{name=From, output=[Id]} | N];
                V = #task{output=L} -> keyreplace(From, #task.id, N, V#task{output=[Id|L]}) end,
    NewN = case keyfind(To, #task.id, N1) of
                false -> [#task{name=To, input=[Id]} | N1];
                V1 = #task{input=L1} -> keyreplace(To, #task.id, N1, V1#task{input=[Id|L1]}) end,
    vertices(Rtail, NewN).

getEdgesAndVertices(Edges_Raw) ->
    R=[#sequenceFlow{name = Id, source=From, target=To} || {Id, From, To} <- Edges_Raw],
    {R,vertices(R)}.

setVertexType(Id, Type, Vertices) ->
    NewVertex = (keyfind(Id, #task.id, Vertices))#gateway{type=Type},
    keyreplace(Id, #sequenceFlow.id, Vertices, NewVertex).

fixBeginEndEvents (Proc) ->
    Tasks = Proc#process.tasks,
    [BeginTask] = [T || #task{input=In}=T <- Tasks, In==[]],
    #task{name=BName, output=_BOut} = BeginTask,
    [EndTask] = [T || #task{output=Out}=T <- Tasks, Out==[]],
    #task{name=EName, input=_EIn, output=[]} = EndTask,
    BeginEvent = #beginEvent{name=BName},
    EndEvent = #endEvent{name=EName},
    Proc#process{tasks = [BeginEvent, EndEvent | Tasks -- [BeginTask,EndTask]],
                 beginEvent = BName,
                 endEvent = EName}.

fixGateways(#process{tasks=Tasks}=Proc) ->
    Proc#process{ tasks = [convertIfNeeded(T) || T <- Tasks ] }.

convertIfNeeded(#task{name=Name, input=In, output=Out})
    when length(In) > 1; length(Out) > 1 -> #gateway{name=Name, type=parallel, input=In, output=Out};
convertIfNeeded(Task) -> Task.

action({request,_,_}, Proc) ->
    {reply,Proc}.
