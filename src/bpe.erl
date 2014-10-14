-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-compile(export_all).

% Instance Management

load(ProcName) -> {ok,Proc} = kvs:get(process,ProcName), Proc.

start(Proc0, Docs) ->
    Proc = case Proc0#process.id of
                undefined -> Id = kvs:next_id("process",1),
                             Proc0#process{id=Id,task=Proc0#process.beginEvent};
                        _ -> Proc0 end,
    kvs:put(Proc),
    Restart = transient,
    Shutdown = 200,
    ChildSpec = { Proc#process.id, 
                  {bpe_proc, start_link, [Proc]},
                  Restart, Shutdown, worker, [bpe_proc] },
    supervisor:start_child(bpe_sup,ChildSpec).

process(ProcId)    -> gen_server:call(ProcId,{get}).
complete(ProcId)   -> gen_server:call(ProcId,{complete}).
amend(ProcId,Docs) -> gen_server:call(ProcId,{amend,Docs}).

delete_tasks(Proc, Tasks) ->
    Proc#process { tasks = [ Task || Task <- Proc#process.tasks,
                                lists:member(Task#task.id,Tasks) ] }.

history(ProcId) -> kvs:entries(kvs:get(feed,{history,ProcId}),history,undefined).

task(Name, Proc) -> 
    case [ Task || Task <- Proc#process.tasks, element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> [];
         E -> E end.

doc(Rec, Proc) ->
    case [ Doc || Doc <- Proc#process.docs, element(1,Doc) == element(1,Rec)] of
         [D] -> D;
         [] -> [];
         E -> E end.

docs(Proc) -> Proc#process.docs.
tasks(Proc) -> Proc#process.tasks.
events(Proc) -> Proc#process.events.

% Process Schema

new_task(Proc,GivenTask) -> 
   Existed = [ Task || Task<- Proc#process.tasks, Task#task.id == GivenTask#task.id],
   case Existed of
        [] -> Proc#process{tasks=[GivenTask|Proc#process.tasks]};
         _ -> {error,exist,Existed} end.

delete(Proc) -> ok.
