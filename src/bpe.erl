-module(bpe).
-author('Maxim Sokhatsky').
-include_lib("bpe/include/bpe.hrl").
-include_lib("bpe/include/api.hrl").
-include_lib("kvs/include/cursors.hrl").
-compile(export_all).
-define(TIMEOUT, application:get_env(bpe,timeout,60000)).
-define(DRIVER,  (application:get_env(bpe,driver,exclusive))).

load(Id) -> load(Id, []).
load(Id, Def) ->
    case application:get_env(kvs,dba,kvs_mnesia) of
         kvs_mnesia -> case kvs:get(process,Id) of
                            {ok,P1} -> P1;
                            {error,_Reason} -> Def end;
         kvs_rocks  -> case kvs:get("/bpe/proc",Id) of
                            {ok,P2} -> P2;
                            {error,Reason} ->
                               io:format("BPE Load Error: ~p~n",[Reason]),
                               Def end end.

cleanup(P) ->
  [ kvs:delete("/bpe/hist",Id) || #hist{id=Id} <- bpe:hist(P) ],
    kvs:delete(writer,"/bpe/hist/" ++ P),
  [ kvs:delete("/bpe/flow",Id) || #sched{id=Id} <- sched(P) ],
    kvs:delete(writer, "/bpe/flow/" ++ P),
    kvs:delete("/bpe/proc",P).

current_task(#process{id=Id}=Proc) ->
    case bpe:head(Id) of
         [] -> {empty,bpe:first_task(Proc)};
         #hist{id={step,H,_},task=T} when is_list(T) -> {H,T}; %% H - ProcId
         #hist{id={step,H,_},task=#sequenceFlow{target=T}} -> {H,T} end. %% H - ProcId

add_trace(Proc,Name,Task) ->
    Key = "/bpe/hist/" ++ Proc#process.id,
    kvs:append(Proc,"/bpe/proc"),
    add_hist(Key,Proc,Name,Task).

add_error(Proc,Name,Task) ->
    io:format("BPE Error for PID ~p:~n~p~n~p~n",[Proc#process.id, Name, Task]),
    Key = "/bpe/error/" ++ Proc#process.id,
    add_hist(Key,Proc,Name,Task).

add_hist(Key,Proc,Name,Task) ->
    Writer = kvs:writer(Key),
    kvs:append(#hist{ id = {step,Writer#writer.count,Proc#process.id},
                    name = Name,
                    time = #ts{ time = calendar:local_time()},
                    docs = Proc#process.docs,
                    task = Task}, Key).

add_sched(Proc,Pointer,State) ->
    Key = "/bpe/flow/" ++ Proc#process.id,
    Writer = kvs:writer(Key),
    kvs:append(#sched{ id = {step,Writer#writer.count,Proc#process.id},
                  pointer = Pointer,
                    state = State}, Key).

start(Proc0, Options) -> start(Proc0, Options, []).

start(Proc0, Options, Monitor) ->
    Id   = case Proc0#process.id of [] -> kvs:seq([],[]); X -> X end,
    {Hist,Task} = current_task(Proc0#process{id=Id}),
    Pid  = proplists:get_value(notification,Options,undefined),
    Proc = Proc0#process{id=Id,
           docs = Options,
           notifications = Pid,
           started= #ts{ time = calendar:local_time() } },

    case Hist of empty -> add_trace(Proc,[],Task),
                          add_sched(Proc,1,[first_flow(Proc)]);
                 _ -> skip end,

    Restart = transient,
    Shutdown = ?TIMEOUT,
    ChildSpec = { Id,
                  {bpe_proc, start_link, [Proc]},
                  Restart, Shutdown, worker, [bpe_proc] },

    case supervisor:start_child(bpe_otp,ChildSpec) of
         {ok,_}    -> supervise(Proc, Monitor), {ok,Proc#process.id};
         {ok,_,_}  -> supervise(Proc, Monitor), {ok,Proc#process.id};
         {error,Reason} -> {error,Reason} end.

supervise(#process{} = Proc, []) -> ok;
supervise(#process{} = Proc, #monitor{} = Monitor) ->
   Key = "/bpe/mon/" ++ Monitor#monitor.id,
   kvs:writer(Key),
   kvs:append(#procRec{id=Proc#process.id,name=Proc#process.name}, Key).

pid(Id) -> bpe:cache({process,Id}).

proc(ProcId)              -> gen_server:call(pid(ProcId),{get},            ?TIMEOUT).
complete(ProcId)          -> gen_server:call(pid(ProcId),{complete},       ?TIMEOUT).
next(ProcId)              -> gen_server:call(pid(ProcId),{next},           ?TIMEOUT).
complete(ProcId,Stage)    -> gen_server:call(pid(ProcId),{complete,Stage}, ?TIMEOUT).
next(ProcId,Stage)        -> gen_server:call(pid(ProcId),{next,Stage},     ?TIMEOUT).
amend(ProcId,Form)        -> gen_server:call(pid(ProcId),{amend,Form},     ?TIMEOUT).
discard(ProcId,Form)      -> gen_server:call(pid(ProcId),{discard,Form},   ?TIMEOUT).
modify(ProcId,Form,Arg)   -> gen_server:call(pid(ProcId),{modify,Form,Arg},?TIMEOUT).
event(ProcId,Event)       -> gen_server:call(pid(ProcId),{event,Event},    ?TIMEOUT).

first_flow(#process{beginEvent = BeginEvent, flows = Flows}) ->
  (lists:keyfind(BeginEvent, #sequenceFlow.source, Flows))#sequenceFlow.id.

first_task(#process{tasks=Tasks}) ->
  case [N || #beginEvent{id=N} <- Tasks] of [] -> []; [Name|_] -> Name end.

head(ProcId) ->
  Key = case application:get_env(kvs,dba,kvs_mnesia) of
             kvs_rocks  -> "/bpe/hist/" ++ ProcId;
             kvs_mnesia -> hist end,
  case kvs:get(writer,"/bpe/hist/" ++ ProcId) of
       {ok, #writer{count = C}} -> case kvs:get(Key,{step,C - 1,ProcId}) of
                                        {ok, X} -> X; _ -> [] end;
                              _ -> [] end.

sched(#step{proc = ProcId}=Step) ->
  Key = case application:get_env(kvs,dba,kvs_mnesia) of
             kvs_rocks  -> "/bpe/flow/" ++ ProcId;
             kvs_mnesia -> sched end,
  case kvs:get(Key,Step) of {ok, X} -> X; _ -> [] end;

sched(ProcId) -> kvs:feed("/bpe/flow/" ++ ProcId).

sched_head(ProcId) ->
  Key = case application:get_env(kvs,dba,kvs_mnesia) of
             kvs_rocks  -> "/bpe/flow/" ++ ProcId;
             kvs_mnesia -> sched end,
  case kvs:get(writer,"/bpe/flow/" ++ ProcId) of
       {ok, #writer{count = C}} -> case kvs:get(Key,{step,C - 1,ProcId}) of
                                        {ok, X} -> X; _ -> [] end;
                              _ -> [] end.

errors(ProcId) -> kvs:feed("/bpe/error/" ++ ProcId).

hist(ProcId)   -> kvs:feed("/bpe/hist/" ++ ProcId).
hist(ProcId,N) ->
  Key =  case application:get_env(kvs,dba,kvs_mnesia) of
              kvs_rocks  -> "/bpe/hist/" ++ ProcId;
              kvs_mnesia -> hist end,
  case kvs:get(Key,{step,N,ProcId}) of
       {ok,Res} -> Res;
       {error,_Reason} -> [] end .

step(Proc,Name) ->
    case [ Task || Task <- tasks(Proc), element(#task.id,Task) == Name] of
         [T] -> T;
         [] -> #task{};
         E -> E end.

docs  (Proc) -> Proc#process.docs.
tasks (Proc) -> Proc#process.tasks.
flows (Proc) -> Proc#process.flows.
events(Proc) -> Proc#process.events.
doc (R,Proc) -> {X,_} = bpe_env:find(env,Proc,R), X.
flow(FlowId,_Proc=#process{flows=Flows}) -> lists:keyfind(FlowId,#sequenceFlow.id,Flows).
flowId(#sched{state=Flows, pointer=N})   -> lists:nth(N, Flows).

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
    case is_atom(TTL) of
        true -> TTL;
        false -> calendar:gregorian_seconds_to_datetime(
                 calendar:datetime_to_gregorian_seconds(Now) + TTL) end.

reload(Module) ->
    {Module, Binary, Filename} = code:get_object_code(Module),
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} ->
            {reloaded, Module};
        {error, Reason} ->
            {load_error, Module, Reason} end.

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

processFlow(ForcedFlowId, #process{}=Proc) ->
    case flow(ForcedFlowId, Proc) of
             false -> add_error(Proc, "No such sequenceFlow", ForcedFlowId),
                      {reply,{error,"No such sequenceFlow",ForcedFlowId},Proc};
        ForcedFlow -> Threads = (sched_head(Proc#process.id))#sched.state,
                      case string:str(Threads,[ForcedFlowId]) of
                           0 -> add_error(Proc,"Unavailable flow",ForcedFlow),
                                {reply,{error,"Unavailable flow",ForcedFlow},Proc};
                  NewPointer -> add_sched(Proc,NewPointer,Threads),
                                add_trace(Proc,"Forced Flow",ForcedFlow),
                                processFlow(Proc) end end.

processFlow(#process{}=Proc) ->
    processSched(sched_head(Proc#process.id),Proc).

processSched(#sched{state=[]},Proc) -> {stop,normal,'Final',Proc};
processSched(#sched{} = Sched,Proc) ->
    Flow = flow(flowId(Sched), Proc),
    SourceTask = lists:keyfind(Flow#sequenceFlow.source, #task.id, tasks(Proc)),
    TargetTask = lists:keyfind(Flow#sequenceFlow.target, #task.id, tasks(Proc)),
    Module = Proc#process.module,
    Autorized = Module:auth(element(#task.roles, SourceTask)),
    processAuthorized(Autorized,SourceTask,TargetTask,Flow,Sched,Proc).

processAuthorized(false,SourceTask,_TargetTask,Flow,_Sched,Proc) ->
    add_error(Proc,"Access denied",Flow),
    {reply, {error, "Access denied", SourceTask}, Proc};
processAuthorized(true,_,Task,Flow,#sched{id=SchedId, pointer=Pointer, state=Threads},Proc) ->
    Inserted = get_inserted(Task, Flow, SchedId, Proc),
    NewThreads = lists:sublist(Threads, Pointer-1) ++ Inserted ++ lists:nthtail(Pointer, Threads),
    NewPointer = if Pointer == length(Threads) -> 1; true -> Pointer + length(Inserted) end,
    add_sched(Proc, NewPointer, NewThreads),
    #sequenceFlow{id=Next, source=Src,target=Dst} = Flow,
    io:format("Flow: ~p~n",[Flow]),
    Resp = {Status,{Reason,_Reply},State}
         = bpe_task:task_action(Proc#process.module,Src,Dst,Proc),
    add_trace(State,[],Flow),
    bpe_proc:debug(State,Next,Src,Dst,Status,Reason),
    Resp.

get_inserted(T,_,_,_) when [] == element(#task.out, T) -> [];
get_inserted(#gateway{id=Name,type=exclusive,out=Out,default=[]},_,_,Proc) ->
  case first_matched_flow(Out,Proc) of
    [] ->
      add_error(Proc,"All conditions evaluate to false in exlusive gateway without default",Name),
      [];
    X -> X end;
get_inserted(#gateway{type=exclusive,out=Out,default=DefFlow},_,_,Proc) ->
  case first_matched_flow(Out--[DefFlow],Proc) of
    [] -> [DefFlow];
    X  -> X end;
get_inserted(#gateway{type=Type,in=In,out=Out},Flow,ScedId,_Proc)
    when Type == inclusive; Type == parallel ->
    case check_all_flows(In -- [Flow#sequenceFlow.id], ScedId) of
         true -> Out;
         false -> [] end;
get_inserted(T,_,_,Proc) -> bpe:?DRIVER(T,Proc).

exclusive(T, Proc) -> first_matched_flow(element(#task.out, T),Proc).
last(T, _Proc)     -> [lists:last(element(#task.out, T))].
first(T, _Proc)    -> [hd(element(#task.out, T))].
random(T, _Proc)   -> Out = element(#task.out, T), [lists:nth(rand:uniform(length(Out)), Out)].

check_all_flows([], _) -> true;
check_all_flows(_, #step{id = -1}) -> false;
check_all_flows(Needed, ScedId=#step{id=Id}) ->
    check_all_flows(Needed -- [flowId(sched(ScedId))], ScedId#step{id = Id-1}).

first_matched_flow([], _Proc) -> [];
first_matched_flow([H | Flows], Proc) ->
    case check_flow_condition(flow(H,Proc),Proc) of
         true -> [H];
         false -> first_matched_flow(Flows, Proc) end.

check_flow_condition(#sequenceFlow{condition=[]},#process{}) -> true;
check_flow_condition(#sequenceFlow{condition={compare,BpeDocParam,Field,ConstCheckAgainst}},Proc) ->
    case doc(BpeDocParam,Proc) of
         [] -> add_error(Proc, "No such document", BpeDocParam), false;
       Docs when is_list(Docs) -> element(Field,hd(Docs)) == ConstCheckAgainst end;
check_flow_condition(#sequenceFlow{condition={service,FunName}},Proc) ->
    Module = Proc#process.module,
    Module:FunName(Proc).
