-module(bpe).
-include("bpe.hrl").
-compile(export_all).

% Instance Management

load(ProcName) -> kvs:get(process,ProcName).
start(Proc0, Docs) ->
    Id = kvs:next_id("process",1),
    Proc = Proc0#process{id=Id},
    wf:info(?MODULE,"BPE Start Process ~p: ",[Proc]),
    Restart = transient,
    Shutdown = 200,
    ChildSpec = {Id, {bpe_proc, start_link, [Proc]}, Restart, Shutdown, worker, [bpe_proc]},
    supervisor:start_child(bpe_sup,ChildSpec).

delete_tasks(Proc, Tasks) ->
    Proc#process { tasks = [ Task || Task <- Proc#process.tasks,
                                lists:member(Task#task.id,Tasks) ] }.

join(Proc) -> ok.
claim(Task, User) -> ok.
complete(Task) -> ok.
resolve(Task) -> ok.
history(Proc) -> ok.
tasks(Proc, Roles) -> ok.
task_event(Task) -> ok.
events(Proc) -> Proc#process.events.

% Process Schema

new_task(Proc,GivenTask) -> 
   Existed = [ Task || Task<- Proc#process.tasks, Task#task.id == GivenTask#task.id],
   case Existed of
        [] -> Proc#process{tasks=[GivenTask|Proc#process.tasks]};
         _ -> {error,exist,Existed} end.

delete(Proc) -> ok.
