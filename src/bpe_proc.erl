-module(bpe_proc).

-author('Maxim Sokhatsky').

-include("bpe.hrl").

-include_lib("kvs/include/cursors.hrl").

-behaviour(gen_server).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_continue/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-export([debug/6,
         process_task/2,
         process_task/3]).

start_link(Parameters) ->
    gen_server:start_link(?MODULE, Parameters, []).

debug(Proc, Name, Targets, Target, Status, Reason) ->
    case application:get_env(bpe, debug, true) of
        true ->
            logger:notice("BPE: ~ts [~ts:~ts] ~p/~p ~p",
                          [Proc#process.id,
                           Name,
                           Target,
                           Status,
                           Reason,
                           Targets]);
        false -> skip
    end.

process_event(EventType, Event, #process{id=Pid, module=Module} = Proc) ->
  Name = kvs:field(Event, name),
  #result{type=T, state = NewProc} = Res =
    case Module:action(Event, Proc) of
      #result{type = reply} = R when EventType == async -> R#result{type = noreply, reply = []};
      #result{} = R when EventType == async -> R#result{reply = []};
      R -> R
    end,
  debug(Proc, atom_to_list(element(1, Event)), "", Name, T, ""),
  kvs:append(NewProc, "/bpe/proc"),
  kvs:remove(Event, bpe:key("/bpe/messages/queue/", Pid)),
  kvs:append(erlang:setelement(2, Event, kvs:seq([], [])), bpe:key("/bpe/messages/hist/", Pid)),
  case Res of
    #result{continue=C} = X when C =/= [] ->
      bpe:constructResult(X#result{opt={continue, C}});
    X -> bpe:constructResult(X)
  end.

process_task(Stage, Proc) ->
    process_task(Stage, Proc, false).

process_task(Stage, Proc, NoFlow) ->
    {_, Curr} = bpe:current_task(Proc),
    Task = bpe:step(Proc, Curr),
    Targets = case NoFlow of
                  true -> noflow;
                  _ -> bpe_task:targets(Curr, Proc)
              end,
    {Status, {Reason, Target}, ProcState} = case {Targets,
                                                  Curr,
                                                  Stage}
                                                of
                                                {noflow, _, _} -> {reply, {complete, Curr}, Proc};
                                                {[], [], _} -> bpe_task:already_finished(Proc);
                                                {[], Curr, _} ->
                                                    bpe_task:handle_task(Task, Curr, Curr, Proc);
                                                {[], _, _} -> bpe_task:denied_flow(Curr, Proc);
                                                {List, _, []} ->
                                                    bpe_task:handle_task(Task,
                                                                         Curr,
                                                                         bpe_task:find_flow(Stage, List),
                                                                         Proc);
                                                {List, _, _} ->
                                                    {reply,
                                                     {complete, bpe_task:find_flow(Stage, List)},
                                                     Proc}
                                            end,
    case Status == stop orelse NoFlow == true of
        true -> {stop, Reason, Target, ProcState};
        _ ->
            bpe:add_trace(ProcState, [], Target),
            debug(ProcState, Curr, Targets, Target, Status, Reason),
            {Status, {Reason, Target}, ProcState}
    end.

behavior_fun_args(handle_call, Req, From, St) -> [Req, From, St];
behavior_fun_args(_, Req, _, St) -> [Req, St].

behavior_fun(asyncEvent, _) -> handle_cast;
behavior_fun(broadcastEvent, {_, broadcastEvent, #broadcastEvent{type = immediate}}) -> handle_cast;
behavior_fun(_, _) -> handle_call.

convert_api_args(proc, [Id, _ProcId]) -> {Id, get};
convert_api_args(update, [Id, _ProcId, State]) -> {Id, set, State};
convert_api_args(assign, [Id, _ProcId]) -> {Id, ensure_mon};
convert_api_args(Fn, [Id, _ | Args]) ->
  list_to_tuple([Id, Fn | Args]).

continueId([#continue{} | _] = X, Id) ->
  lists:map(fun (C) -> C#continue{id = Id} end, X);
continueId(_, _) -> [].

delete_lock(Id, #terminateLock{messages = M} = L, Pid) ->
  bpe:cache(terminateLocks, {terminateLock, Pid}, L#terminateLock{messages = lists:filter(fun(X) -> X =/= Id end, M)}).

handle_result(call, {noreply, S}) -> {reply, ok, S};
handle_result(call, {noreply, S, X}) -> {reply, ok, S, X};
handle_result(_, X) -> X.
handle_result(T, Id, X, #process{id = Pid} = DefState) ->
  handle_result(T, terminate_check(Id, X, bpe:cache(terminateLocks, {terminateLock, Pid}), DefState)).

terminate_check(_, _, #terminateLock{limit = L, counter = C}, #process{id = Pid} = DefState) when C >= L ->
  bpe:cache(terminateLocks, {terminate, Pid}, true), {stop, normal, DefState};
terminate_check(Id, X, #terminateLock{messages=[I]}, #process{id = Pid}) when Id == I ->
  bpe:cache(terminateLocks, {terminateLock, Pid}, undefined),
  bpe:cache(terminateLocks, {terminate, Pid}, erlang:element(1, X) == stop),
  X;
terminate_check(Id, {stop, normal, S}, #terminateLock{} = L, #process{id = Pid}) ->
  delete_lock(Id, L, Pid), {noreply, S};
terminate_check(Id, {stop, normal, Reply, S}, #terminateLock{} = L, #process{id = Pid}) ->
  delete_lock(Id, L, Pid), {reply, Reply, S};
terminate_check(_, X, _, #process{id = Pid}) ->
  bpe:cache(terminateLocks, {terminate, Pid}, erlang:element(1, X) == stop), X.

handleContinue({noreply, State}, Continue, Id) when Continue =/= [] ->
  {noreply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({reply, Reply, State, {continue, C}}, Continue, Id) ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue) ++ C, Id)}};
handleContinue({reply, Reply, State, _}, Continue, Id) when Continue =/= [] ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({noreply, State, {continue, C}}, Continue, Id) ->
  {noreply, State, {continue, continueId(lists:flatten(Continue) ++ C, Id)}};
handleContinue({noreply, State, _}, Continue, Id) when Continue =/= [] ->
  {noreply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({reply, Reply, State}, Continue, Id) when Continue =/= [] ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({stop, _, State}, Continue, Id) when Continue =/= [] ->
  {noreply, State, {continue, continueId(lists:flatten(Continue) ++ [#continue{type=stop}], Id)}};
handleContinue({stop, _, Reply, State}, Continue, Id) when Continue =/= [] ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue) ++ [#continue{type=stop}], Id)}};
handleContinue(X, _, _) -> X.


% BPMN 2.0 Инфотех
handle_call({Id, mon_link, MID}, _, Proc) ->
    ProcNew = Proc#process{monitor = MID},
    handle_result(call, Id, {reply, ProcNew, ProcNew}, Proc);
handle_call({Id, ensure_mon}, _, Proc) ->
    {Mon, ProcNew} = bpe:ensure_mon(Proc),
    handle_result(call, Id, {stop, normal, Mon, ProcNew}, Proc);
handle_call({Id, get}, _, Proc) -> handle_result(call, Id, {stop, normal, Proc, Proc}, Proc);
handle_call({Id, set, State}, _, Proc) ->
    handle_result(call, Id, {stop, normal, Proc, State}, State);
handle_call({Id, persist, State}, _, #process{} = _Proc) ->
    kvs:append(State, "/bpe/proc"),
    handle_result(call, Id, {stop, normal, State, State}, State);
handle_call({Id, next}, _, #process{} = Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(Proc), [], Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'next/1', Z}, {error, 'next/1', Z}, Proc}
    end;
handle_call({Id, next, [#continue{} | _] = Continue}, _, #process{} = Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(Proc), Continue, Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'next/1', Z}, {error, 'next/1'}, Proc}
    end;
handle_call({Id, next, Stage}, _, Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(Stage, Proc), [], Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'next/2', Z}, {error, 'next/2', Z}, Proc}
    end;
handle_call({Id, amend, Form}, _, Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(bpe_env:append(env, Proc, Form)), [], Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'amend/2', Z}, {error, 'amend/2', Z}, Proc}
    end;
handle_call({Id, discard, Form}, _, Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(bpe_env:remove(env, Proc, Form)), [], Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'amend/2', Z}, {error, 'discard/2', Z}, Proc}
    end;
handle_call({Id, messageEvent, Event}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_event(sync, Event, Proc), [], Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'messageEvent/2', Z}, {error, 'messageEvent/2', Z}, Proc}
    end;
handle_call({Id, messageEvent, Event, [#continue{} | _] = Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_event(sync, Event, Proc), Continue, Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'messageEvent/3', Z}, {error, 'messageEvent/3', Z}, Proc}
    end;
% BPMN 1.0 ПриватБанк
handle_call({Id, complete}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task([], Proc), [], Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'complete/1', Z}, {error, 'complete/1', Z}, Proc}
    end;
handle_call({Id, complete, [#continue{} | _] = Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task([], Proc), Continue, Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'complete/2', Z}, {error, 'complete/2', Z}, Proc}
    end;
handle_call({Id, complete, Stage}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task(Stage, Proc), [], Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'complete/2', Z}, {error, 'complete/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, append}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task([],
                     bpe_env:append(env, Proc, Form),
                     true), [], Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'append/2', Z}, {error, 'append/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, remove}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task([],
                     bpe_env:remove(env, Proc, Form),
                     true), [], Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'remove/2', Z}, {error, 'remove/2', Z}, Proc}
    end;

handle_call({Id, mon_link, MID, Continue}, _, Proc) ->
    ProcNew = Proc#process{monitor = MID},
    handle_result(call, Id, handleContinue({stop, normal, ProcNew, ProcNew}, Continue, Id), Proc);
handle_call({Id, ensure_mon, Continue}, _, Proc) ->
    {Mon, ProcNew} = bpe:ensure_mon(Proc),
    handle_result(call, Id, handleContinue({stop, normal, Mon, ProcNew}, Continue, Id), Proc);
handle_call({Id, set, State, Continue}, _, Proc) ->
    handle_result(call, Id, handleContinue({stop, normal, Proc, State}, Continue, Id), State);
handle_call({Id, persist, State, Continue}, _, #process{} = _Proc) ->
    kvs:append(State, "/bpe/proc"),
    handle_result(call, Id, handleContinue({stop, normal, State, State}, Continue, Id), State);
handle_call({Id, next, Stage, Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(Stage, Proc), Continue, Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'next/2', Z}, {error, 'next/2', Z}, Proc}
    end;
handle_call({Id, amend, Form, Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(bpe_env:append(env, Proc, Form)), Continue, Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'amend/2', Z}, {error, 'amend/2', Z}, Proc}
    end;
handle_call({Id, discard, Form, Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(bpe:processFlow(bpe_env:remove(env, Proc, Form)), Continue, Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'discard/2', Z}, {error, 'discard/2', Z}, Proc}
    end;
handle_call({Id, complete, Stage, Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task(Stage, Proc), Continue, Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'complete/2', Z}, {error, 'complete/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, append, Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task([],
                     bpe_env:append(env, Proc, Form),
                     true), Continue, Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'append/2', Z}, {error, 'append/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, remove, Continue}, _, Proc) ->
    try handle_result(call, Id, handleContinue(process_task([],
                     bpe_env:remove(env, Proc, Form),
                     true), Continue, Id), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'remove/2', Z}, {error, 'remove/2', Z}, Proc}
    end;
handle_call(Command, _, Proc) ->
    handle_result(call, kvs:seq([], []), {stop, unknown, {unknown, Command}, Proc}, Proc).

handle_continue([#continue{type=spawn, module=Module, fn=Fn, args=Args} | T], Proc) ->
  spawn(fun() -> apply(Module, Fn, Args) end),
  {noreply, Proc, {continue, T}};
handle_continue([#continue{id = Id, type=bpe, fn=Fn, args=Args} | T], Proc) ->
    BpeArgs = convert_api_args(Fn, [Id | Args]),
    Result = try apply(bpe_proc, behavior_fun(Fn, BpeArgs), behavior_fun_args(behavior_fun(Fn, BpeArgs), BpeArgs, [], Proc))
             catch
               _X:_Y:Z -> {stop, {error, Z}, Proc}
             end,
    case Result of
      {noreply, State} ->
        {noreply, State, {continue, T}};
      {reply, _, State, {continue, C}} ->
        {noreply, State, {continue, T ++ continueId(C, Id)}};
      {reply, _, State, _} ->
        {noreply, State, {continue, T}};
      {noreply, State, {continue, C}} ->
        {noreply, State, {continue, T ++ continueId(C, Id)}};
      {noreply, State, _} ->
        {noreply, State, {continue, T}};
      {reply, _, State} ->
        {noreply, State, {continue, T}};
      {stop, _, State} ->
        {noreply, State, {continue, T ++ [#continue{id = Id, type=stop}]}};
      {stop, _, _, State} ->
        {noreply, State, {continue, T ++ [#continue{id = Id, type=stop}]}};
      X -> X
    end;
handle_continue([#continue{id=Id, type=stop}], Proc) ->
    handle_result(continue, Id, {stop, normal, Proc}, Proc);
handle_continue([#continue{type=stop} = X | T], Proc) ->
    case lists:keymember(stop, #continue.type, T) of
      true -> {noreply, Proc, {continue, T}};
      _ -> {noreply, Proc, {continue, T ++ [X]}}
    end;
handle_continue([], Proc) ->
    handle_result(continue, [], {stop, normal, Proc}, Proc).

init(Process) ->
    process_flag(trap_exit, true),
    Proc = bpe:load(Process#process.id, Process),
    logger:notice("BPE: ~ts spawned as ~p",
                  [Proc#process.id, self()]),
    Till = bpe:till(calendar:local_time(),
                    application:get_env(bpe, ttl, 24 * 60 * 60)),
    bpe:cache(processes, {process, Proc#process.id}, self(), Till),
    [bpe:reg({messageEvent,
              element(1, EventRec),
              Proc#process.id})
     || EventRec <- bpe:events(Proc)],
    {ok,
     Proc#process{timer =
                      erlang:send_after(rand:uniform(10000),
                                        self(),
                                        {timer, ping})}}.

handle_cast({Id, asyncEvent, Event}, Proc) ->
    try handle_result(cast, Id, handleContinue(process_event(async, Event, Proc), [], Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'asyncEvent/2', Z}, Proc}
    end;
handle_cast({Id, asyncEvent, Event, [#continue{} | _] = Continue}, Proc) ->
    try handle_result(cast, Id, handleContinue(process_event(async, Event, Proc), Continue, Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'asyncEvent/3', Z}, Proc}
    end;
handle_cast({Id, broadcastEvent, Event}, Proc) ->
    try handle_result(cast, Id, handleContinue(process_event(async, Event, Proc), [], Id), Proc) catch
        _X:_Y:Z -> {stop, {error, 'broadcastEvent/2', Z}, Proc}
    end;

handle_cast(Msg, State) ->
    logger:notice("BPE: Unknown API async: ~p.", [Msg]),
    handle_result(cast, kvs:seq([], []), {stop, {error, {unknown_cast, Msg}}, State}, State).

handle_info({timer, ping},
            State = #process{timer = _Timer, id = Id,
                             events = _Events, notifications = _Pid}) ->
    lists:foreach(fun (Event) ->
      try process_event(async, Event, State) catch
        _X:_Y:Z -> Z
      end
    end, kvs:all(bpe:key("/bpe/messages/queue/", Id))),
    case application:get_env(bpe, ping_discipline, bpe_ping) of
      undefined -> {noreply, State};
      M -> M:ping(State)
    end;

handle_info({'DOWN', _, _, _, _} = Msg, State) ->
    logger:notice("BPE: Connection closed, shutting down "
                  "session: ~p.",
                  [Msg]),
    handle_result(info, kvs:seq([], []), {stop, normal, State}, State);
handle_info({'EXIT', _, Reason} = Msg, State) ->
    logger:notice("BPE EXIT: ~p.", [Msg]),
    handle_result(info, kvs:seq([], []), {stop, Reason, State}, State);
handle_info(Info, State = #process{}) ->
    logger:notice("BPE: Unrecognized info: ~p", [Info]),
    handle_result(info, kvs:seq([], []), {stop, unknown_info, State}, State).

terminate(Reason, #process{id = Id} = Proc) ->
    bpe:cache(processes, {process, Id}, undefined),
    bpe:cache(terminateLocks, {terminateLock, Id}, undefined),
    logger:notice("BPE: ~ts terminate Reason: ~p", [Id, Reason]),
    lists:foreach(fun (Event) ->
      try process_event(async, Event, Proc) catch
        _X:_Y:Z -> Z
      end
    end, kvs:all(bpe:key("/bpe/messages/queue/", Id))),
    bpe:cache(terminateLocks, {terminate, Id}, undefined),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
