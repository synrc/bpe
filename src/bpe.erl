-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("api.hrl").
-compile(export_all).

% Instance Management

load(ProcName) -> {ok,Proc} = kvs:get(process,ProcName), Proc.

cleanup(P) -> [ kvs:remove(history,Id) || #history{id=Id} <- bpe:history(P) ],
                kvs:delete(feed,{history,P}),
                kvs:remove(process,P).

start(Proc0, Options) ->
    Pid = proplists:get_value(notification,Options,undefined),
    Proc = case Proc0#process.id of
                undefined -> Id = kvs:next_id("process",1),
                             Proc0#process{id=Id,task=Proc0#process.beginEvent,
                                           options = Options,
                                           notifications = Pid};
                        _ -> Proc0 end,
    kvs:add(Proc),
    Restart = transient,
    Shutdown = 5000,
    ChildSpec = { Proc#process.id,
                  {bpe_proc, start_link, [Proc]},
                  Restart, Shutdown, worker, [bpe_proc] },
    case supervisor:start_child(bpe_sup,ChildSpec) of
         {ok,_}   -> {ok,Proc#process.id};
         {ok,_,_} -> {ok,Proc#process.id};
         Else     -> Else end.

find_pid(Id) -> wf:cache({process,Id}).

process(ProcId)           -> gen_server:call(find_pid(ProcId),{get}).
complete(ProcId)          -> gen_server:call(find_pid(ProcId),{complete}).
run(ProcId)               -> gen_server:call(find_pid(ProcId),{run}).
until(ProcId,Task)        -> gen_server:call(find_pid(ProcId),{until,Task}).
complete(Stage,ProcId)    -> gen_server:call(find_pid(ProcId),{complete,Stage}).
amend(ProcId,Form)        -> gen_server:call(find_pid(ProcId),{amend,Form}).
amend(ProcId,Form,noflow) -> gen_server:call(find_pid(ProcId),{amend,Form,true}).
event(ProcId,Event)       -> gen_server:call(find_pid(ProcId),{event,Event}).

addRecsProc(Proc, RecordsList) -> bpe_proc:set_rec_in_proc(Proc, RecordsList).

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

option(Proc, Key) -> proplists:get_value(Key,Proc#process.options).
option(Proc, Key, Value) -> Proc#process{options=bpe_proc:plist_setkey(Key,1,Proc#process.options,{Key,Value})}.

