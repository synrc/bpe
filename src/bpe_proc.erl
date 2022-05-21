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
  kvs:append(Event, bpe:key("/bpe/messages/hist/", Pid)),
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

convert_api_args(proc, [_ProcId]) -> {get};
convert_api_args(update, [_ProcId, State]) -> {set, State};
convert_api_args(assign, [_ProcId]) -> {ensure_mon};
convert_api_args(Fn, [_ | Args]) ->
  list_to_tuple([Fn | Args]).

handleContinue({noreply, State}, Continue) ->
  {noreply, State, {continue, lists:flatten(Continue)}};
handleContinue({reply, Reply, State, {continue, C}}, Continue) ->
  {reply, Reply, State, {continue, lists:flatten(Continue) ++ C}};
handleContinue({reply, Reply, State, _}, Continue) ->
  {reply, Reply, State, {continue, lists:flatten(Continue)}};
handleContinue({noreply, State, {continue, C}}, Continue) ->
  {noreply, State, {continue, lists:flatten(Continue) ++ C}};
handleContinue({noreply, State, _}, Continue) ->
  {noreply, State, {continue, lists:flatten(Continue)}};
handleContinue({reply, Reply, State}, Continue) ->
  {reply, Reply, State, {continue, lists:flatten(Continue)}};
handleContinue({stop, _, State}, Continue) ->
  {noreply, State, {continue, lists:flatten(Continue) ++ [#continue{type=stop}]}};
handleContinue({stop, _, Reply, State}, Continue) ->
  {reply, Reply, State, {continue, lists:flatten(Continue) ++ [#continue{type=stop}]}};
handleContinue(X, _) -> X.


% BPMN 2.0 Инфотех
handle_call({mon_link, MID}, _, Proc) ->
    ProcNew = Proc#process{monitor = MID},
    {reply, ProcNew, ProcNew};
handle_call({ensure_mon}, _, Proc) ->
    {Mon, ProcNew} = bpe:ensure_mon(Proc),
    {reply, Mon, ProcNew};
handle_call({get}, _, Proc) -> {reply, Proc, Proc};
handle_call({set, State}, _, Proc) ->
    {reply, Proc, State};
handle_call({persist, State}, _, #process{} = _Proc) ->
    kvs:append(State, "/bpe/proc"),
    {reply, State, State};
handle_call({next}, _, #process{} = Proc) ->
    try bpe:processFlow(Proc) catch
        _X:_Y:Z -> {reply, {error, 'next/1', Z}, Proc}
    end;
handle_call({next, [#continue{} | _] = Continue}, _, #process{} = Proc) ->
    try handleContinue(bpe:processFlow(Proc), Continue) catch
        _X:_Y:Z -> {reply, {error, 'next/1', Z}, Proc, {continue, Continue}}
    end;
handle_call({next, Stage}, _, Proc) ->
    try bpe:processFlow(Stage, Proc) catch
        _X:_Y:Z -> {reply, {error, 'next/2', Z}, Proc}
    end;
handle_call({amend, Form}, _, Proc) ->
    try bpe:processFlow(bpe_env:append(env, Proc, Form))
    catch
        _X:_Y:Z -> {reply, {error, 'amend/2', Z}, Proc}
    end;
handle_call({discard, Form}, _, Proc) ->
    try bpe:processFlow(bpe_env:remove(env, Proc, Form))
    catch
        _X:_Y:Z -> {reply, {error, 'discard/2', Z}, Proc}
    end;
handle_call({messageEvent, Event}, _, Proc) ->
    try process_event(sync, Event, Proc) catch
        _X:_Y:Z -> {reply, {error, 'messageEvent/2', Z}, Proc}
    end;
handle_call({messageEvent, Event, [#continue{} | _] = Continue}, _, Proc) ->
    try handleContinue(process_event(sync, Event, Proc), Continue) catch
        _X:_Y:Z -> {reply, {error, 'messageEvent/3', Z}, Proc}
    end;
% BPMN 1.0 ПриватБанк
handle_call({complete}, _, Proc) ->
    try process_task([], Proc) catch
        _X:_Y:Z -> {reply, {error, 'complete/1', Z}, Proc}
    end;
handle_call({complete, [#continue{} | _] = Continue}, _, Proc) ->
    try handleContinue(process_task([], Proc), Continue) catch
        _X:_Y:Z -> {reply, {error, 'complete/1', Z}, Proc, {continue, Continue}}
    end;
handle_call({complete, Stage}, _, Proc) ->
    try process_task(Stage, Proc) catch
        _X:_Y:Z -> {reply, {error, 'complete/2', Z}, Proc}
    end;
handle_call({modify, Form, append}, _, Proc) ->
    try process_task([],
                     bpe_env:append(env, Proc, Form),
                     true)
    catch
        _X:_Y:Z -> {reply, {error, 'append/2', Z}, Proc}
    end;
handle_call({modify, Form, remove}, _, Proc) ->
    try process_task([],
                     bpe_env:remove(env, Proc, Form),
                     true)
    catch
        _X:_Y:Z -> {reply, {error, 'remove/2', Z}, Proc}
    end;

handle_call({mon_link, MID, Continue}, _, Proc) ->
    ProcNew = Proc#process{monitor = MID},
    {reply, ProcNew, ProcNew, {continue, Continue}};
handle_call({ensure_mon, Continue}, _, Proc) ->
    {Mon, ProcNew} = bpe:ensure_mon(Proc),
    {reply, Mon, ProcNew, {continue, Continue}};
handle_call({set, State, Continue}, _, Proc) ->
    {reply, Proc, State, {continue, Continue}};
handle_call({persist, State, Continue}, _, #process{} = _Proc) ->
    kvs:append(State, "/bpe/proc"),
    {reply, State, State, {continue, Continue}};
handle_call({next, Stage, Continue}, _, Proc) ->
    try handleContinue(bpe:processFlow(Stage, Proc), Continue) catch
        _X:_Y:Z -> {reply, {error, 'next/2', Z}, Proc, {continue, Continue}}
    end;
handle_call({amend, Form, Continue}, _, Proc) ->
    try handleContinue(bpe:processFlow(bpe_env:append(env, Proc, Form)), Continue)
    catch
        _X:_Y:Z -> {reply, {error, 'amend/2', Z}, Proc, {continue, Continue}}
    end;
handle_call({discard, Form, Continue}, _, Proc) ->
    try handleContinue(bpe:processFlow(bpe_env:remove(env, Proc, Form)), Continue)
    catch
        _X:_Y:Z -> {reply, {error, 'discard/2', Z}, Proc, {continue, Continue}}
    end;
handle_call({complete, Stage, Continue}, _, Proc) ->
    try handleContinue(process_task(Stage, Proc), Continue) catch
        _X:_Y:Z -> {reply, {error, 'complete/2', Z}, Proc, {continue, Continue}}
    end;
handle_call({modify, Form, append, Continue}, _, Proc) ->
    try handleContinue(process_task([],
                     bpe_env:append(env, Proc, Form),
                     true), Continue)
    catch
        _X:_Y:Z -> {reply, {error, 'append/2', Z}, Proc, {continue, Continue}}
    end;
handle_call({modify, Form, remove, Continue}, _, Proc) ->
    try handleContinue(process_task([],
                     bpe_env:remove(env, Proc, Form),
                     true), Continue)
    catch
        _X:_Y:Z -> {reply, {error, 'remove/2', Z}, Proc, {continue, Continue}}
    end;
handle_call(Command, _, Proc) ->
    {reply, {unknown, Command}, Proc}.


handle_continue([#continue{type=spawn, module=Module, fn=Fn, args=Args} | T], #process{} = Proc) ->
  spawn(fun() -> apply(Module, Fn, Args) end),
  {noreply, Proc, {continue, T}};
handle_continue([#continue{type=bpe, fn=Fn, args=Args} | T], #process{} = Proc) ->
    Result = try handle_call(convert_api_args(Fn, Args), [], Proc)
             catch
               _X:_Y:Z -> {stop, {error, Z}, Proc}
             end,
    case Result of
      {noreply, State} ->
        {noreply, State, {continue, T}};
      {reply, _, State, {continue, C}} ->
        {noreply, State, {continue, T ++ C}};
      {reply, _, State, _} ->
        {noreply, State, {continue, T}};
      {noreply, State, {continue, C}} ->
        {noreply, State, {continue, T ++ C}};
      {noreply, State, _} ->
        {noreply, State, {continue, T}};
      {reply, _, State} ->
        {noreply, State, {continue, T}};
      {stop, _, State} ->
        {noreply, State, {continue, T ++ [#continue{type=stop}]}};
      {stop, _, _, State} ->
        {noreply, State, {continue, T ++ [#continue{type=stop}]}};
      X -> X
    end;
handle_continue([#continue{type=stop}], #process{} = Proc) ->
    {stop, normal, Proc};
handle_continue([#continue{type=stop} = X | T], #process{} = Proc) ->
    case lists:member(#continue{type=stop}, T) of
      true -> {noreply, Proc, {continue, T}};
      _ -> {noreply, Proc, {continue, T ++ [X]}}
    end;
handle_continue([], #process{} = Proc) ->
    {noreply, Proc}.

init(Process) ->
    Proc = bpe:load(Process#process.id, Process),
    logger:notice("BPE: ~ts spawned as ~p",
                  [Proc#process.id, self()]),
    Till = bpe:till(calendar:local_time(),
                    application:get_env(bpe, ttl, 24 * 60 * 60)),
    bpe:cache({process, Proc#process.id}, self(), Till),
    [bpe:reg({messageEvent,
              element(1, EventRec),
              Proc#process.id})
     || EventRec <- bpe:events(Proc)],
    {ok,
     Proc#process{timer =
                      erlang:send_after(rand:uniform(10000),
                                        self(),
                                        {timer, ping})}}.

handle_cast({asyncEvent, Event}, Proc) ->
    try process_event(async, Event, Proc) catch
        _X:_Y:Z -> {stop, {error, 'asyncEvent/2', Z}, Proc}
    end;
handle_cast({asyncEvent, Event, [#continue{} | _] = Continue}, Proc) ->
    try handleContinue(process_event(async, Event, Proc), Continue) catch
        _X:_Y:Z -> {stop, {error, 'asyncEvent/3', Z}, Proc}
    end;
handle_cast({broadcastEvent, Event}, Proc) ->
    try process_event(async, Event, Proc) catch
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
    bpe:cache({process, Id}, undefined),
    {stop, normal, State};
handle_info(Info, State = #process{}) ->
    logger:notice("BPE: Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #process{id = Id} = Proc) ->
    lists:foreach(fun (Event) ->
      try process_event(async, Event, Proc) catch
        _X:_Y:Z -> Z
      end
    end, kvs:all(bpe:key("/bpe/messages/queue/", Id))),
    logger:notice("BPE: ~ts terminate Reason: ~p",
                  [Id, Reason]),
    bpe:cache({process, Id}, undefined),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
