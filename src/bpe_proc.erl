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
      #result{} = R when EventType == async -> R#result{type = noreply, reply = []};
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

convert_api_args(proc, [Id, _ProcId]) -> {Id, get};
convert_api_args(update, [Id, _ProcId, State]) -> {Id, set, State};
convert_api_args(assign, [Id, _ProcId]) -> {Id, ensure_mon};
convert_api_args(Fn, [Id, _ | Args]) ->
  list_to_tuple([Id, Fn | Args]).

continueId([#continue{} | _] = X, Id) ->
  lists:map(fun (C) -> C#continue{id = Id} end, X);
continueId(_, _) -> [].

terminate_check(Id, X, #process{id = Pid} = DefState) ->
  terminate_check(Id, X, bpe:cache(terminateLocks, {terminateLock, Pid}), DefState).
terminate_check(_, X, #terminateLock{limit = L, counter = C}, DefState) when C >= L ->
  {stop, normal, DefState};
terminate_check(Id, {reply, _, _, {continue, _}} = X, #terminateLock{id = I}, _) when Id == I ->
  X;
terminate_check(Id, {noreply, _, {continue, _}} = X, #terminateLock{id = I}, _) when Id == I ->
  X;
terminate_check(Id, X, #terminateLock{id = I}, #process{id = Pid}) when Id == I ->
  bpe:cache(terminateLocks, {terminateLock, Pid}, undefined),
  X;
terminate_check(_, {stop, normal, S}, #terminateLock{}, _) ->
  {noreply, S};
terminate_check(_, {stop, normal, Reply, S}, #terminateLock{}, _) ->
  {reply, Reply, S};
terminate_check(_, X, _, _) -> X.

handleContinue({noreply, State}, Continue, Id) ->
  {noreply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({reply, Reply, State, {continue, C}}, Continue, Id) ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue) ++ C, Id)}};
handleContinue({reply, Reply, State, _}, Continue, Id) ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({noreply, State, {continue, C}}, Continue, Id) ->
  {noreply, State, {continue, continueId(lists:flatten(Continue) ++ C, Id)}};
handleContinue({noreply, State, _}, Continue, Id) ->
  {noreply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({reply, Reply, State}, Continue, Id) ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue), Id)}};
handleContinue({stop, _, State}, Continue, Id) ->
  {noreply, State, {continue, continueId(lists:flatten(Continue) ++ [#continue{type=stop}], Id)}};
handleContinue({stop, _, Reply, State}, Continue, Id) ->
  {reply, Reply, State, {continue, continueId(lists:flatten(Continue) ++ [#continue{type=stop}], Id)}};
handleContinue(X, _, _) -> X.


% BPMN 2.0 Инфотех
handle_call({_, mon_link, MID}, _, Proc) ->
    ProcNew = Proc#process{monitor = MID},
    {reply, ProcNew, ProcNew};
handle_call({Id, ensure_mon}, _, Proc) ->
    {Mon, ProcNew} = bpe:ensure_mon(Proc),
    terminate_check(Id, {stop, normal, Mon, ProcNew}, Proc);
handle_call({Id, get}, _, Proc) -> terminate_check(Id, {stop, normal, Proc, Proc}, Proc);
handle_call({_, set, State}, _, Proc) ->
    {stop, normal, Proc, State};
handle_call({_, persist, State}, _, #process{} = _Proc) ->
    kvs:append(State, "/bpe/proc"),
    {stop, normal, State, State};
handle_call({Id, next}, _, #process{} = Proc) ->
    try terminate_check(Id, bpe:processFlow(Proc), Proc) catch
        _X:_Y:Z -> {stop, {error, 'next/1', Z}, {error, 'next/1', Z}, Proc}
    end;
handle_call({Id, next, [#continue{} | _] = Continue}, _, #process{} = Proc) ->
    try handleContinue(bpe:processFlow(Proc), Continue, Id) catch
        _X:_Y:Z -> {stop, {error, 'next/1', Z}, {error, 'next/1'}, Proc}
    end;
handle_call({Id, next, Stage}, _, Proc) ->
    try terminate_check(Id, bpe:processFlow(Stage, Proc), Proc) catch
        _X:_Y:Z -> {stop, {error, 'next/2', Z}, {error, 'next/2', Z}, Proc}
    end;
handle_call({Id, amend, Form}, _, Proc) ->
    try terminate_check(Id, bpe:processFlow(bpe_env:append(env, Proc, Form)), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'amend/2', Z}, {error, 'amend/2', Z}, Proc}
    end;
handle_call({Id, discard, Form}, _, Proc) ->
    try terminate_check(Id, bpe:processFlow(bpe_env:remove(env, Proc, Form)), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'amend/2', Z}, {error, 'discard/2', Z}, Proc}
    end;
handle_call({Id, messageEvent, Event}, _, Proc) ->
    logger:notice("HANDLE CALL MESSAGE EVENT 0: ~tp ~tp", [Proc#process.id, Id]),
    X1 = process_event(sync, Event, Proc),
    logger:notice("HANDLE CALL MESSAGE EVENT 1: ~tp ~tp ~tp", [Proc#process.id, Id, X1]),
    X2 = handleContinue(X1, [], Id),
    logger:notice("HANDLE CALL MESSAGE EVENT 2: ~tp ~tp ~tp", [Proc#process.id, Id, X2]),
    X = try terminate_check(Id, X2, Proc) catch
        _X:_Y:Z -> {stop, {error, 'messageEvent/2', Z}, {error, 'messageEvent/2', Z}, Proc}
    end,
    logger:notice("HANDLE CALL MESSAGE EVENT 3: ~tp ~tp ~tp", [Proc#process.id, Id, X]),
    X;
handle_call({Id, messageEvent, Event, [#continue{} | _] = Continue}, _, Proc) ->
    try handleContinue(process_event(sync, Event, Proc), Continue, Id) catch
        _X:_Y:Z -> {stop, {error, 'messageEvent/3', Z}, {error, 'messageEvent/3', Z}, Proc}
    end;
% BPMN 1.0 ПриватБанк
handle_call({Id, complete}, _, Proc) ->
    try terminate_check(Id, process_task([], Proc), Proc) catch
        _X:_Y:Z -> {stop, {error, 'complete/1', Z}, {error, 'complete/1', Z}, Proc}
    end;
handle_call({Id, complete, [#continue{} | _] = Continue}, _, Proc) ->
    try handleContinue(process_task([], Proc), Continue, Id) catch
        _X:_Y:Z -> {stop, {error, 'complete/2', Z}, {error, 'complete/2', Z}, Proc}
    end;
handle_call({Id, complete, Stage}, _, Proc) ->
    try terminate_check(Id, process_task(Stage, Proc), Proc) catch
        _X:_Y:Z -> {stop, {error, 'complete/2', Z}, {error, 'complete/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, append}, _, Proc) ->
    try terminate_check(Id, process_task([],
                     bpe_env:append(env, Proc, Form),
                     true), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'append/2', Z}, {error, 'append/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, remove}, _, Proc) ->
    try terminate_check(Id, process_task([],
                     bpe_env:remove(env, Proc, Form),
                     true), Proc)
    catch
        _X:_Y:Z -> {stop, {error, 'remove/2', Z}, {error, 'remove/2', Z}, Proc}
    end;

handle_call({Id, mon_link, MID, Continue}, _, Proc) ->
    ProcNew = Proc#process{monitor = MID},
    handleContinue({stop, normal, ProcNew, ProcNew}, Continue, Id);
handle_call({Id, ensure_mon, Continue}, _, Proc) ->
    {Mon, ProcNew} = bpe:ensure_mon(Proc),
    handleContinue({stop, normal, Mon, ProcNew}, Continue, Id);
handle_call({Id, set, State, Continue}, _, Proc) ->
    handleContinue({stop, normal, Proc, State}, Continue, Id);
handle_call({Id, persist, State, Continue}, _, #process{} = _Proc) ->
    kvs:append(State, "/bpe/proc"),
    handleContinue({stop, normal, State, State}, Continue, Id);
handle_call({Id, next, Stage, Continue}, _, Proc) ->
    try handleContinue(bpe:processFlow(Stage, Proc), Continue, Id) catch
        _X:_Y:Z -> {stop, {error, 'next/2', Z}, {error, 'next/2', Z}, Proc}
    end;
handle_call({Id, amend, Form, Continue}, _, Proc) ->
    try handleContinue(bpe:processFlow(bpe_env:append(env, Proc, Form)), Continue, Id)
    catch
        _X:_Y:Z -> {stop, {error, 'amend/2', Z}, {error, 'amend/2', Z}, Proc}
    end;
handle_call({Id, discard, Form, Continue}, _, Proc) ->
    try handleContinue(bpe:processFlow(bpe_env:remove(env, Proc, Form)), Continue, Id)
    catch
        _X:_Y:Z -> {stop, {error, 'discard/2', Z}, {error, 'discard/2', Z}, Proc}
    end;
handle_call({Id, complete, Stage, Continue}, _, Proc) ->
    try handleContinue(process_task(Stage, Proc), Continue, Id) catch
        _X:_Y:Z -> {stop, {error, 'complete/2', Z}, {error, 'complete/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, append, Continue}, _, Proc) ->
    try handleContinue(process_task([],
                     bpe_env:append(env, Proc, Form),
                     true), Continue, Id)
    catch
        _X:_Y:Z -> {stop, {error, 'append/2', Z}, {error, 'append/2', Z}, Proc}
    end;
handle_call({Id, modify, Form, remove, Continue}, _, Proc) ->
    try handleContinue(process_task([],
                     bpe_env:remove(env, Proc, Form),
                     true), Continue, Id)
    catch
        _X:_Y:Z -> {stop, {error, 'remove/2', Z}, {error, 'remove/2', Z}, Proc}
    end;
handle_call(Command, _, Proc) ->
    {stop, unknown, {unknown, Command}, Proc}.

handle_continue([#continue{type=spawn, module=Module, fn=Fn, args=Args} | T], #process{id = Pid} = Proc) ->
  logger:notice("HANDLE CONTINUE 6 ~tp ~tp", [Pid, bpe:cache(terminateLocks, {terminateLock, Pid})]),
  spawn(fun() -> apply(Module, Fn, Args) end),
  {noreply, Proc, {continue, T}};
handle_continue([#continue{id = Id, type=bpe, fn=Fn, args=Args} | T], #process{id = Pid} = Proc) ->
    logger:notice("HANDLE CONTINUE 1 ~tp ~tp ~tp", [Id, Pid, convert_api_args(Fn, [Id | Args])]),
    Result = try handle_call(convert_api_args(Fn, [Id | Args]), [], Proc)
             catch
               _X:_Y:Z -> {stop, {error, Z}, Proc}
             end,
    logger:notice("HANDLE CONTINUE 2 ~tp ~tp ~tp", [Id, Pid, Result]),
    R = case Result of
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
    end,
    logger:notice("HANDLE CONTINUE 3 ~tp ~tp ~tp", [Id, Pid, setelement(2, R, [])]),
    R;
handle_continue([#continue{id=Id, type=stop}], #process{id = Pid} = Proc) ->
    logger:notice("HANDLE CONTINUE STOP 1 ~tp ~tp ~tp", [Id, Pid, bpe:cache(terminateLocks, {terminateLock, Pid})]),
    X = terminate_check(Id, {stop, normal, Proc}, Proc),
    logger:notice("HANDLE CONTINUE STOP 2 ~tp ~tp ~tp ~tp", [Id, Pid, bpe:cache(terminateLocks, {terminateLock, Pid}), X]),
    X;
handle_continue([#continue{type=stop} = X | T], #process{id = Pid} = Proc) ->
    logger:notice("HANDLE CONTINUE 4 ~tp ~tp ~tp", [Pid, bpe:cache(terminateLocks, {terminateLock, Pid}), T]),
    case lists:filter(fun (#continue{type=Type}) -> Type == stop end, T) =/= [] of
      true -> {noreply, Proc, {continue, T}};
      _ -> {noreply, Proc, {continue, T ++ [X]}}
    end;
handle_continue([], #process{id = Pid} = Proc) ->
    logger:notice("HANDLE CONTINUE 5 ~tp ~tp", [Pid, bpe:cache(terminateLocks, {terminateLock, Pid})]),
    {noreply, Proc}.

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
    try terminate_check(Id, process_event(async, Event, Proc), Proc) catch
        _X:_Y:Z -> {stop, {error, 'asyncEvent/2', Z}, Proc}
    end;
handle_cast({Id, asyncEvent, Event, [#continue{} | _] = Continue}, Proc) ->
    try handleContinue(process_event(async, Event, Proc), Continue, Id) catch
        _X:_Y:Z -> {stop, {error, 'asyncEvent/3', Z}, Proc}
    end;
handle_cast({Id, broadcastEvent, Event}, Proc) ->
    try terminate_check(Id, process_event(async, Event, Proc), Proc) catch
        _X:_Y:Z -> {stop, {error, 'broadcastEvent/2', Z}, Proc}
    end;

handle_cast(Msg, State) ->
    logger:notice("BPE: Unknown API async: ~p.", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

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

handle_info({'DOWN',
             _MonitorRef,
             _Type,
             _Object,
             _Info} =
                Msg,
            State = #process{id = Id}) ->
    logger:notice("BPE: Connection closed, shutting down "
                  "session: ~p.",
                  [Msg]),
    terminate_check(kvs:seq([], []), {stop, normal, State}, State);
handle_info({'EXIT', _, Reason} = Msg, State = #process{id = Id}) ->
    logger:notice("BPE EXIT: ~p.", [Msg]),
    terminate_check(kvs:seq([], []), {stop, Reason, State}, State);
handle_info(Info, State = #process{}) ->
    logger:notice("BPE: Unrecognized info: ~p", [Info]),
    {stop, unknown_info, State}.

terminate(Reason, #process{id = Id} = Proc) ->
    lists:foreach(fun (Event) ->
      try process_event(async, Event, Proc) catch
        _X:_Y:Z -> Z
      end
    end, kvs:all(bpe:key("/bpe/messages/queue/", Id))),
    logger:notice("BPE: ~ts terminate Reason: ~p",
                  [Id, Reason]),
    bpe:cache(terminateLocks, {terminateLock, Id}, undefined),
    bpe:cache(processes, {process, Id}, undefined),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
