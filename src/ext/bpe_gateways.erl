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



%%vertices(R,N=[]) -> N
vertices(R) -> vertices(reverse(R),[]).
vertices([], N) -> N;
vertices([#sequenceFlow{name=Id, source=From, target=To} | Rtail], N) ->
   N1 = case keyfind(From, #task.name, N) of
            false -> [#task{name=From, out=[Id], module=?MODULE} | N];
            V = #task{out=L} -> keyreplace(From, #task.name, N, V#task{out=[Id|L]})
        end,
   NewN = case keyfind(To, #task.name, N1) of
              false -> [#task{name=To, in=[Id], module=?MODULE} | N1];
              V1 = #task{in=L1} -> keyreplace(To, #task.name, N1, V1#task{in=[Id|L1]})
          end,
   vertices(Rtail, NewN).

getEdgesAndVertices(Edges_Raw) ->
  R=[#sequenceFlow{name = Id, source=From, target=To} || {Id, From, To} <- Edges_Raw],
  {R,vertices(R)}.

setVertexType(Id, Type, Vertices) ->
  NewVertex = (keyfind(Id, #task.name, Vertices))#gateway{type=Type}, %should be #gateway{type=Type}
  keyreplace(Id, #sequenceFlow.name, Vertices, NewVertex).

fixBeginEndEvents (Proc) ->
  Tasks = Proc#process.tasks,
  [BeginTask] = [T || #task{in=In}=T <- Tasks, In==[]],
  #task{name=BName, out=_BOut, module=Module} = BeginTask,
  [EndTask] = [T || #task{out=Out}=T <- Tasks, Out==[]],
  #task{name=EName, in=_EIn, out=[], module=Module} = EndTask,
  BeginEvent = #beginEvent{name=BName, module=Module},
  EndEvent = #endEvent{name=EName, module=Module},
  Proc#process{tasks = [BeginEvent, EndEvent | Tasks -- [BeginTask,EndTask]],
               beginEvent = BName,
               endEvent = EName}.

fixGateways(#process{tasks=Tasks}=Proc) -> %%Tasks should contain correct in/out
  Proc#process{ tasks = [convertIfNeeded(T) || T <- Tasks ] }.

convertIfNeeded(#task{name=Name, in=In, out=Out, module=Module}) when length(In)>1; length(Out)>1 -> #gateway{name=Name, type=parallel, inputs=In, outputs=Out, module=Module};
convertIfNeeded(Task) -> Task.

action({request,_,_}, Proc) ->
    {reply,Proc}.
