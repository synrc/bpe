-module(bpe).

-author('Maxim Sokhatsky').

-include_lib("bpe/include/bpe.hrl").

-include_lib("bpe/include/api.hrl").

-include_lib("kvs/include/cursors.hrl").

-export([load/1,
         load/2,
         cleanup/1,
         current_task/1,
         add_trace/3,
         add_error/3,
         add_sched/3,
         add_hist/4]).

-export([start/2,
         start/3,
         mon_link/3,
         mon_children/1,
         pid/1,
         ensure_mon/1,
         proc/1,
         update/2,
         persist/2]).

-export([key/1,
         key/2,
         check_flow_condition/2,
         first_matched_flow/2,
         check_all_flows/2,
         random/2,
         first/2,
         last/2,
         exclusive/2]).

-export([get_inserted/4,
         processAuthorized/6,
         processSched/2,
         processFlow/1,
         processFlow/2,
         reg/1,
         reg/2,
         unreg/1,
         send/2,
         reload/1,
         ttl/0,
         till/2]).

-export([assign/1,
         complete/1,
         next/1,
         complete/2,
         next/2,
         amend/2,
         discard/2,
         modify/3,
         event/2,
         first_flow/1,
         first_task/1]).

-export([head/1,
         hist/1,
         sched_head/1,
         step/2,
         docs/1,
         doc/2,
         errors/1,
         tasks/1,
         flows/1,
         events/1,
         flow/2,
         flowId/1]).

-export([cache/1, cache/2, cache/3]).

-define(TIMEOUT,
        application:get_env(bpe, timeout, 6000)).

-define(DRIVER,
        application:get_env(bpe, driver, exclusive)).

-define(API,
        (application:get_env(bpe, api, bpe_api))).

load(X) -> ?API:load(X).
load(X, Y) -> ?API:load(X, Y).
add_hist(A, B, C, D) -> ?API:add_hist(A, B, C, D).
start(X, Y) -> ?API:start(X, Y).
proc(X) -> ?API:proc(X).
update(X, Y) -> ?API:update(X, Y).
persist(X, Y) -> ?API:persist(X, Y).
assign(X) -> ?API:assign(X).
complete(X) -> ?API:complete(X).
complete(X, Y) -> ?API:complete(X, Y).
next(X) -> ?API:next(X).
next(X, Y) -> ?API:next(X, Y).
amend(X, Y) -> ?API:amend(X, Y).
discard(X, Y) -> ?API:discard(X, Y).
modify(X, Y, Z) -> ?API:modify(X, Y, Z).
event(X, Y) -> ?API:event(X, Y).
head(X) -> ?API:head(X).
hist(X) -> ?API:hist(X).
docs(X) -> ?API:docs(X).
doc(X, Y) -> ?API:doc(X, Y).
check_flow_condition(X, Y) -> ?API:check_flow_condition(X, Y).

cleanup(P) ->
    [kvs:delete("/bpe/hist", Id)
     || #hist{id = Id} <- bpe:hist(P)],
    kvs:delete(writer, key("/bpe/hist/", P)),
    [kvs:delete("/bpe/flow", Id)
     || #sched{id = Id} <- sched(P)],
    kvs:delete(writer, key("/bpe/flow/", P)),
    kvs:delete("/bpe/proc", P).

current_task(#process{id = Id} = Proc) ->
    case bpe:head(Id) of
        [] -> {empty, bpe:first_task(Proc)};
        #hist{id = {step, H, _},
              task = #sequenceFlow{target = T}} ->
            {H, T}; %% H - ProcId
        #hist{id = {step, H, _}, task = T} ->
            {H, T} %% H - ProcId
    end.

add_trace(Proc, Name, Task) ->
    Key = key("/bpe/hist/", Proc#process.id),
    add_hist(Key, Proc, Name, Task).

add_error(Proc, Name, Task) ->
    logger:notice("BPE: Error for PID ~ts: ~p ~p",
                  [Proc#process.id, Name, Task]),
    Key = key("/bpe/error/", Proc#process.id),
    add_hist(Key, Proc, Name, Task).

add_sched(Proc, Pointer, State) ->
    Key = key("/bpe/flow/", Proc#process.id),
    Writer = kvs:writer(Key),
    kvs:append(#sched{id =
                          key({step, Writer#writer.count, Proc#process.id}),
                      pointer = Pointer, state = State},
               Key).

start(Proc0, Options, {Monitor, ProcRec}) ->
    Id = iolist_to_binary([case Proc0#process.id of
                               [] -> kvs:seq([], []);
                               X -> X
                           end]),
    {Hist, Task} = current_task(Proc0#process{id = Id}),
    Pid = proplists:get_value(notification,
                              Options,
                              undefined),
    Proc = Proc0#process{id = Id, docs = Options,
                         notifications = Pid,
                         modified = #ts{time = calendar:local_time()},
                         started = #ts{time = calendar:local_time()}},
    case Hist of
        empty ->
            add_trace(Proc, [], Task),
            add_sched(Proc, 1, [first_flow(Proc)]);
        _ -> skip
    end,
    Restart = transient,
    Shutdown = (?TIMEOUT),
    ChildSpec = {Id,
                 {bpe_proc, start_link, [Proc]},
                 Restart,
                 Shutdown,
                 worker,
                 [bpe_proc]},
    case supervisor:start_child(bpe_otp, ChildSpec) of
        {ok, _} ->
            mon_link(Monitor, Proc, ProcRec),
            {ok, Id};
        {ok, _, _} ->
            mon_link(Monitor, Proc, ProcRec),
            {ok, Id};
        {error, Reason} -> {error, Reason}
    end.

% monitors

mon_link(Mon, Proc, ProcRec) ->
    mon_link(Mon, Proc, ProcRec, false).

mon_link([], Proc, _, _) ->
    kvs:append(Proc, "/bpe/proc");
mon_link(#monitor{id = MID} = Monitor, Proc, ProcRec,
         Embedded) ->
    Key = key("/bpe/mon/", MID),
    case kvs:get(writer, Key) of
        {error, _} -> kvs:append(Monitor, "/bpe/monitors");
        {ok, _} -> skip
    end,
    ProcId = Proc#process.id,
    MemoProc = case Embedded of
                   false -> gen_server:call(pid(ProcId), {mon_link, MID});
                   true -> Proc#process{monitor = MID}
               end,
    kvs:append(Proc#process{monitor = MID}, "/bpe/proc"),
    kvs:append(ProcRec#procRec{id = ProcId}, Key),
    MemoProc.

mon_children(MID) -> kvs:all(key("/bpe/mon/", MID)).

pid(Id) -> bpe:cache({process, iolist_to_binary([Id])}).

ensure_mon(#process{monitor = [], id = Id} = Proc) ->
    Mon = #monitor{id = kvs:seq([], [])},
    ProcRec = #procRec{id = []},
    {Mon,
     mon_link(Mon, Proc, ProcRec#procRec{id = Id}, true)};
ensure_mon(#process{monitor = MID} = Proc) ->
    case kvs:get("/bpe/monitors", MID) of
        {error, X} -> throw({error, X});
        {ok, Mon} -> {Mon, Proc}
    end.

first_flow(#process{beginEvent = BeginEvent,
                    flows = Flows}) ->
    (lists:keyfind(BeginEvent,
                   #sequenceFlow.source,
                   Flows))#sequenceFlow.id.

first_task(#process{tasks = Tasks}) ->
    case [N || #beginEvent{id = N} <- Tasks] of
        [] -> [];
        [Name | _] -> Name
    end.

sched(#step{proc = ProcId} = Step) ->
    Key = case application:get_env(kvs, dba, kvs_mnesia) of
              kvs_rocks -> key("/bpe/flow/", ProcId);
              kvs_mnesia -> sched
          end,
    case kvs:get(Key, Step) of
        {ok, X} -> X;
        _ -> []
    end;
sched(ProcId) -> kvs:all(key("/bpe/flow/", ProcId)).

sched_head(ProcId) ->
    Key = case application:get_env(kvs, dba, kvs_mnesia) of
              kvs_rocks -> key("/bpe/flow/", ProcId);
              kvs_mnesia -> sched
          end,
    case kvs:get(writer, key("/bpe/flow/", ProcId)) of
        {ok, #writer{count = C}} ->
            case kvs:get(Key, key({step, C - 1, ProcId})) of
                {ok, X} -> X;
                _ -> []
            end;
        _ -> []
    end.

errors(ProcId) -> kvs:all(key("/bpe/error/", ProcId)).

step(Proc, Name) ->
    case [Task
          || Task <- tasks(Proc), element(#task.id, Task) == Name]
        of
        [T] -> T;
        [] -> #task{};
        E -> E
    end.

tasks(Proc) -> Proc#process.tasks.

flows(Proc) -> Proc#process.flows.

events(Proc) -> Proc#process.events.

flow(FlowId, _Proc = #process{flows = Flows}) ->
    lists:keyfind(FlowId, #sequenceFlow.id, Flows).

flowId(#sched{state = Flows, pointer = N}) ->
    lists:nth(N, Flows).

cache(Key, undefined) -> ets:delete(processes, Key);
cache(Key, Value) ->
    ets:insert(processes,
               {Key, till(calendar:local_time(), ttl()), Value}),
    Value.

cache(Key, Value, Till) ->
    ets:insert(processes, {Key, Till, Value}),
    Value.

cache(Key) ->
    Res = ets:lookup(processes, Key),
    Val = case Res of
              [] -> undefined;
              [Value] -> Value;
              Values -> Values
          end,
    case Val of
        undefined -> undefined;
        {_, infinity, X} -> X;
        {_, Expire, X} ->
            case Expire < calendar:local_time() of
                true ->
                    ets:delete(processes, Key),
                    undefined;
                false -> X
            end
    end.

ttl() -> application:get_env(bpe, ttl, 60 * 15).

till(Now, TTL) ->
    case is_atom(TTL) of
        true -> TTL;
        false ->
            calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(Now)
                                                       + TTL)
    end.

reload(Module) ->
    {Module, Binary, Filename} =
        code:get_object_code(Module),
    case code:load_binary(Module, Filename, Binary) of
        {module, Module} -> {reloaded, Module};
        {error, Reason} -> {load_error, Module, Reason}
    end.

send(Pool, Message) ->
    syn:publish(term_to_binary(Pool), Message).

reg(Pool) -> reg(Pool, undefined).

reg(Pool, Value) ->
    case get({pool, Pool}) of
        undefined ->
            syn:register(term_to_binary(Pool), self(), Value),
            syn:join(term_to_binary(Pool), self()),
            erlang:put({pool, Pool}, Pool);
        _Defined -> skip
    end.

unreg(Pool) ->
    case get({pool, Pool}) of
        undefined -> skip;
        _Defined ->
            syn:leave(Pool, self()),
            erlang:erase({pool, Pool})
    end.

processFlow(ForcedFlowId, #process{} = Proc) ->
    case flow(ForcedFlowId, Proc) of
        false ->
            add_error(Proc, "No such sequenceFlow", ForcedFlowId),
            {reply,
             {error, "No such sequenceFlow", ForcedFlowId},
             Proc};
        ForcedFlow ->
            Threads = (sched_head(Proc#process.id))#sched.state,
            case string:str(Threads, [ForcedFlowId]) of
                0 ->
                    add_error(Proc, "Unavailable flow", ForcedFlow),
                    {reply, {error, "Unavailable flow", ForcedFlow}, Proc};
                NewPointer ->
                    add_sched(Proc, NewPointer, Threads),
                    add_trace(Proc, "Forced Flow", ForcedFlow),
                    processFlow(Proc)
            end
    end.

processFlow(#process{} = Proc) ->
    processSched(sched_head(Proc#process.id), Proc).

processSched(#sched{state = []}, Proc) ->
    {stop, normal, 'Final', Proc};
processSched(#sched{} = Sched, Proc) ->
    Flow = flow(flowId(Sched), Proc),
    SourceTask = lists:keyfind(Flow#sequenceFlow.source,
                               #task.id,
                               tasks(Proc)),
    TargetTask = lists:keyfind(Flow#sequenceFlow.target,
                               #task.id,
                               tasks(Proc)),
    Module = Proc#process.module,
    Autorized = Module:auth(element(#task.roles,
                                    SourceTask)),
    processAuthorized(Autorized,
                      SourceTask,
                      TargetTask,
                      Flow,
                      Sched,
                      Proc).

processAuthorized(false, SourceTask, _TargetTask, Flow,
                  _Sched, Proc) ->
    add_error(Proc, "Access denied", Flow),
    {reply, {error, "Access denied", SourceTask}, Proc};
processAuthorized(true, _, Task, Flow,
                  #sched{id = SchedId, pointer = Pointer,
                         state = Threads},
                  Proc) ->
    Inserted = get_inserted(Task, Flow, SchedId, Proc),
    NewThreads = lists:sublist(Threads, Pointer - 1) ++
                     Inserted ++ lists:nthtail(Pointer, Threads),
    NewPointer = if Pointer == length(Threads) -> 1;
                    true -> Pointer + length(Inserted)
                 end,
    #sequenceFlow{id = Next, source = Src, target = Dst} =
        Flow,
    case application:get_env(bpe, debug, true) of
        true -> skip; % logger:notice("BPE: Flow ~p", [Flow]);
        false -> skip
    end,
    Resp = {Status, {Reason, _Reply}, State} =
               bpe_task:task_action(Proc#process.module,
                                    Src,
                                    Dst,
                                    Proc),
    add_sched(Proc, NewPointer, NewThreads),
    add_trace(State, [], Flow),
    bpe_proc:debug(State, Next, Src, Dst, Status, Reason),
    Resp.

get_inserted(T, _, _, _)
    when [] == element(#task.output, T) ->
    [];
get_inserted(#gateway{id = Name, type = exclusive,
                      output = Out, def = []},
             _, _, Proc) ->
    case first_matched_flow(Out, Proc) of
        [] ->
            add_error(Proc,
                      "All conditions evaluate to false in "
                      "exlusive gateway without default",
                      Name),
            [];
        X -> X
    end;
get_inserted(#gateway{type = exclusive, output = Out,
                      def = DefFlow},
             _, _, Proc) ->
    case first_matched_flow(Out -- [DefFlow], Proc) of
        [] -> [DefFlow];
        X -> X
    end;
get_inserted(#gateway{type = Type, input = In,
                      output = Out},
             Flow, ScedId, _Proc)
    when Type == inclusive; Type == parallel ->
    case check_all_flows(In -- [Flow#sequenceFlow.id],
                         ScedId)
        of
        true -> Out;
        false -> []
    end;
get_inserted(T, _, _, Proc) -> bpe:(?DRIVER)(T, Proc).

exclusive(T, Proc) ->
    first_matched_flow(element(#task.output, T), Proc).

last(T, _Proc) ->
    [lists:last(element(#task.output, T))].

first(T, _Proc) -> [hd(element(#task.output, T))].

random(T, _Proc) ->
    Out = element(#task.output, T),
    [lists:nth(rand:uniform(length(Out)), Out)].

check_all_flows([], _) -> true;
check_all_flows(_, #step{id = 0}) -> false;
check_all_flows(Needed, ScedId = #step{id = Id}) ->
    case hist(ScedId) of
        #hist{task = #sequenceFlow{id = Fid}} ->
            check_all_flows(Needed -- [Fid],
                            ScedId#step{id = Id - 1});
        _ -> false
    end.

first_matched_flow([], _Proc) -> [];
first_matched_flow([H | Flows], Proc) ->
    case check_flow_condition(flow(H, Proc), Proc) of
        true -> [H];
        false -> first_matched_flow(Flows, Proc)
    end.

% temp
key({step, N, [208 | _] = Pid}) ->
    {step, N, list_to_binary(Pid)};
key(Pid) -> Pid.

key(Prefix, Pid) -> iolist_to_binary([Prefix, Pid]).
