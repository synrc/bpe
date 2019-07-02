-module(bpe).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-include("api.hrl").
-export([head/1]).
-compile(export_all).
-define(TIMEOUT, application:get_env(bpe,timeout,60000)).

load(#process{id = ProcName}) -> {ok,Proc} = kvs:get("/bpe/proc",ProcName), Proc;
load(ProcName) -> {ok,Proc} = kvs:get("/bpe/proc",ProcName), Proc.

cleanup(P) ->
  [ kvs:delete({hist,P},Id) || #hist{id=Id} <- bpe:hist(P) ],
    kvs:delete(writer,"/bpe/hist/" ++ P),
    kvs:delete(process,P).

start(Proc0, Options) ->
    Pid = proplists:get_value(notification,Options,undefined),
    Proc = case Proc0#process.id == [] of
                true -> Id = kvs:seq([],[]),
                      Proc0#process{id=Id,task=Proc0#process.beginEvent,
                                    options = Options,notifications = Pid,
                                    started=calendar:local_time()};
                 _ -> Proc0#process{started=calendar:local_time()} end,

    kvs:append(Proc, "/bpe/proc"),
    Key = "/bpe/hist/" ++ Proc#process.id,
    kvs:ensure(#writer{id=Key}),
    kvs:append(#hist{ id = {0,Proc#process.id},
                    name = Proc#process.name,
                    time = Proc#process.started,
                    docs = Proc#process.docs,
                    task = { event, 'Created' }}, Key),

    Restart = transient,
    Shutdown = ?TIMEOUT,
    ChildSpec = { Proc#process.id,
                  {bpe_proc, start_link, [Proc]},
                  Restart, Shutdown, worker, [bpe_proc] },

    case supervisor:start_child(bpe_otp,ChildSpec) of
         {ok,_}    -> {ok,Proc#process.id};
         {ok,_,_}  -> {ok,Proc#process.id};
         {error,_} -> {error,Proc#process.id} end.

find_pid(Id) -> bpe:cache({process,Id}).

proc(ProcId)              -> gen_server:call(find_pid(ProcId),{get},            ?TIMEOUT).
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

% BPE for now supports only MNESIA and ROCKS backends.


head(ProcId) ->
  {ok, #writer{count = C}} = kvs:get(writer,"/bpe/hist/" ++ ProcId),
  {ok, X} = kvs:get("/bpe/hist/" ++ ProcId,{C - 1,ProcId}),
  X.

hist(ProcId)   -> kvs:feed("/bpe/hist/" ++ ProcId).
hist(ProcId,N) -> case application:get_env(kvs,dba,kvs_mnesia) of
                       kvs_mnesia -> case kvs:get(hist,{N,ProcId}) of
                                          {ok,Res} -> Res;
                                          {error,_Reason} -> [] end;
                       kvs_rocks  -> case kvs:get("/bpe/hist/" ++ ProcId,{N,ProcId}) of
                                          {ok,Res} -> Res;
                                          {error,_Reason} -> [] end end .

source(Name, Proc) ->
    case [ Task || Task <- events(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> #beginEvent{};
         E -> E end.

step(Name, Proc) -> 
    case [ Task || Task <- tasks(Proc), element(#task.name,Task) == Name] of
         [T] -> T;
         [] -> #task{};
         E -> E end.

doc(Rec, Proc) ->
    case [ Doc || Doc <- docs(Proc), element(1,Doc) == element(1,Rec)] of
         [D] -> D;
         [] -> [];
         E -> E end.

docs  (Proc) -> Proc#process.docs.
tasks (Proc) -> Proc#process.tasks.
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

ttl() -> application:get_env(bpe,ttl,60*15).

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

reload(Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} ->
            {reloaded, Module};
        {error, Reason} ->
            {load_error, Module, Reason}
    end.

