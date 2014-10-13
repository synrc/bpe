-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-compile(export_all).

% Instance Management

load(ProcName) -> {ok,Proc} = kvs:get(process,ProcName), Proc.

start(Proc0, Docs) ->
    Id = kvs:next_id("process",1),
    Proc = Proc0#process{id=Id},
    kvs:put(Proc),
    Restart = transient,
    Shutdown = 200,
    ChildSpec = {Id, {bpe_proc, start_link, [Proc]}, Restart, Shutdown, worker, [bpe_proc]},
    supervisor:start_child(bpe_sup,ChildSpec).

complete(ProcId) -> gen_server:call(ProcId,{complete}).
amend(Proc,Docs) -> gen_server:call(Proc#process.id,{amend,Docs}).

delete_tasks(Proc, Tasks) ->
    Proc#process { tasks = [ Task || Task <- Proc#process.tasks,
                                lists:member(Task#task.id,Tasks) ] }.

history(ProcId) -> kvs:entries(kvs:get(feed,{history,ProcId}),history,undefined).

task(Name, Proc) -> [T] = [ Task || Task <- Proc#process.tasks, element(#task.name,Task) == Name], T.
doc(Rec, Proc) -> [D] = [ Doc || Doc <- Proc#process.docs, element(1,Doc) == element(1,Rec)], D.

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
