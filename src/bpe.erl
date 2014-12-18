-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("api.hrl").
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

find_pid(Id) -> wf:cache({process,Id}).

process(ProcId) when is_pid(ProcId) -> gen_server:call(ProcId,{get});
process(ProcId) -> gen_server:call(find_pid(ProcId),{get}).
complete(ProcId) when is_pid(ProcId) -> gen_server:call(ProcId,{complete});
complete(ProcId) -> gen_server:call(find_pid(ProcId),{complete}).
complete(Stage,ProcId) when is_pid(ProcId) -> gen_server:call(ProcId,{complete,Stage});
complete(Stage,ProcId) -> gen_server:call(find_pid(ProcId),{complete,Stage}).
amend(ProcId,Form) when is_pid(ProcId) -> gen_server:call(ProcId,{amend,Form});
amend(ProcId,Form) -> gen_server:call(find_pid(ProcId),{amend,Form}).
amend(ProcId,Form,noflow) when is_pid(ProcId) -> gen_server:call(ProcId,{amend,Form,true});
amend(ProcId,Form,noflow) -> gen_server:call(find_pid(ProcId),{amend,Form,true}).
event(ProcId,Event) when is_pid(ProcId) -> gen_server:call(ProcId,{event,Event});
event(ProcId,Event) -> gen_server:call(find_pid(ProcId),{event,Event}).

complete_while(ProcId) ->
    Status = case complete(ProcId) of
                {complete,S} -> S;
                {{complete,_},S} -> S end,
    Status2 = case complete(ProcId) of
                  {complete,S2} -> S2;
                  {{complete,_},S2} -> S2 end,
    case Status == Status2 of
         true -> {complete,Status2};
            _ -> complete_while(ProcId) end.

amend_while(ProcId,Form) ->
    {complete,Status} = amend(ProcId,Form),
    {complete,Status2} = complete(ProcId),
    case Status == Status2 of
         true -> {complete,Status2};
            _ -> complete_while(ProcId) end.

delete_tasks(Proc, Tasks) ->
    Proc#process { tasks = [ Task || Task <- Proc#process.tasks,
                                lists:member(Task#task.name,Tasks) ] }.

history(ProcId) -> kvs:entries(kvs:get(feed,{history,ProcId}),history,undefined).

source(Name, Proc) ->
    case [ Task || Task <- events(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> [];
         E -> E end.

task(Name, Proc) -> 
    case [ Task || Task <- tasks(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> [];
         E -> E end.

doc(Rec, Proc) ->
    case [ Doc || Doc <- docs(Proc), element(1,Doc) == element(1,Rec)] of
         [D] -> D;
         [] -> [];
         E -> E end.

docs(Proc) -> Proc#process.docs.
tasks(Proc) -> Proc#process.tasks.
events(Proc) -> Proc#process.events.

% Process Schema

new_task(Proc,GivenTask) ->
   Existed = [ Task || Task<- Proc#process.tasks, Task#task.name == GivenTask#task.name],
   case Existed of
        [] -> Proc#process{tasks=[GivenTask|Proc#process.tasks]};
         _ -> {error,exist,Existed} end.

delete(Proc) -> ok.

val(Document,Proc,Cond) -> val(Document,Proc,Cond,fun(X,Y)-> ok end).
val(Document,Proc,Cond,Action) ->
    case Cond(Document,Proc) of
         true -> Action(Document,Proc), {reply,Proc};
         {false,Message} -> {{reply,Message},Proc#process.task,Proc};
         ErrorList -> io:format("BPE:val/4 failed: ~p~n",[ErrorList]),
                      {{reply,ErrorList},Proc#process.task,Proc} end.

