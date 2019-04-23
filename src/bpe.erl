-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("api.hrl").
-compile(export_all).
-define(TIMEOUT, application:get_env(bpe,timeout,60000)).

load(ProcName) -> {ok,Proc} = kvs:get(process,ProcName), Proc.

cleanup(P) -> [ kvs:remove(hist,Id) || #hist{id=Id} <- bpe:hist(P) ],
                kvs:delete(feed,{hist,P}),
                kvs:remove(process,P).

start(Proc0, Options) ->
    Pid = proplists:get_value(notification,Options,undefined),
    Proc = case Proc0#process.id == [] of
                true -> Id = kvs:next_id("process",1),
                      Proc0#process{id=Id,task=Proc0#process.beginEvent,
                                    options = Options,notifications = Pid,
                                    started=calendar:local_time()};
                 _ -> Proc0#process{started=calendar:local_time()} end,
    kvs:add(Proc),
    Restart = transient,
    Shutdown = ?TIMEOUT,
    ChildSpec = { Proc#process.id,
                  {bpe_proc, start_link, [Proc]},
                  Restart, Shutdown, worker, [bpe_proc] },

    case supervisor:start_child(bpe_sup,ChildSpec) of
         {ok,_}    -> {ok,Proc#process.id};
         {ok,_,_}  -> {ok,Proc#process.id};
         {error,_} -> {error,Proc#process.id} end.

find_pid(Id) -> bpe:cache({process,Id}).

process(ProcId)           -> gen_server:call(find_pid(ProcId),{get},            ?TIMEOUT).
complete(ProcId)          -> gen_server:call(find_pid(ProcId),{complete},       ?TIMEOUT).
run(ProcId)               -> gen_server:call(find_pid(ProcId),{run},            ?TIMEOUT).
until(ProcId,Task)        -> gen_server:call(find_pid(ProcId),{until,Task},     ?TIMEOUT).
complete(Stage,ProcId)    -> gen_server:call(find_pid(ProcId),{complete,Stage}, ?TIMEOUT).
amend(ProcId,Form)        -> gen_server:call(find_pid(ProcId),{amend,Form},     ?TIMEOUT).
amend(ProcId,Form,noflow) -> gen_server:call(find_pid(ProcId),{amend,Form,true},?TIMEOUT).
event(ProcId,Event)       -> gen_server:call(find_pid(ProcId),{event,Event},    ?TIMEOUT).

delete_tasks(Proc, Tasks) ->
    Proc#process { tasks = [ Task || Task <- Proc#process.tasks,
                                lists:member(Task#task.name,Tasks) ] }.

hist(ProcId)   -> hist(ProcId,undefined).
hist(ProcId,N) -> case kvs:entries(kvs:get(feed,{hist,ProcId}),hist,N) of
                          [] -> [#hist{time=(bpe:load(ProcId))#process.started}];
                          Res -> Res end.

source(Name, Proc) ->
    case [ Task || Task <- events(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> #beginEvent{};
         E -> E end.

task(Name, Proc) -> 
    case [ Task || Task <- tasks(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> #task{};
         E -> E end.

doc(Rec, Proc) ->
    case [ Doc || Doc <- docs(Proc), kvs:rname(element(1,Doc)) == element(1,Rec)] of
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

delete(_Proc) -> ok.

val(Document,Proc,Cond) -> val(Document,Proc,Cond,fun(_,_)-> ok end).
val(Document,Proc,Cond,Action) ->
    case Cond(Document,Proc) of
         true -> Action(Document,Proc), {reply,Proc};
         {false,Message} -> {{reply,Message},Proc#process.task,Proc};
         ErrorList -> io:format("BPE:val/4 failed: ~tp~n",[ErrorList]),
                      {{reply,ErrorList},Proc#process.task,Proc} end.

cache(Key, undefined) -> ets:delete(processes,Key);
cache(Key, Value) -> ets:insert(processes,{Key,till(calendar:local_time(), ttl()),Value}), Value.
cache(Key, Value, Till) -> ets:insert(processes,{Key,Till,Value}), Value.
cache(Key) ->
    Res = ets:lookup(processes,Key),
    Val = case Res of [] -> undefined; [Value] -> Value; Values -> Values end,
    case Val of undefined -> undefined;
                {_,infinity,X} -> X;
                {_,Expire,X} -> case Expire < calendar:local_time() of
                                  true ->  ets:delete(processes,Key), undefined;
                                  false -> X end end.

ttl() -> kvs:config(n2o,ttl,60*15).

till(Now,TTL) ->
    calendar:gregorian_seconds_to_datetime(
        calendar:datetime_to_gregorian_seconds(Now) + TTL).

send(Pool, Message) -> syn:publish(term_to_binary(Pool),Message).
reg(Pool) -> reg(Pool,undefined).
reg(Pool, Value) ->
    case get({pool,Pool}) of
         undefined -> syn:register(term_to_binary(Pool),self(),Value),
                      syn:join(term_to_binary(Pool),self()),
                      erlang:put({pool,Pool},Pool);
          _Defined -> skip end.
unreg(Pool) ->
    case get({pool,Pool}) of
         undefined -> skip;
          _Defined -> syn:leave(Pool, self()),
                      erlang:erase({pool,Pool}) end.
