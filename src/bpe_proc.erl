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
         process_event/2,
         process_task/2,
         process_task/3,
         fix_reply/1]).

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

process_event(Event, Proc) ->
    EventName = element(#messageEvent.id, Event),
    Targets = bpe_task:targets(EventName, Proc),
    Target0 = bpe_task:find_flow(EventName, Targets),
    Target1 = case Target0 of
                  [] -> EventName;
                  T -> T
              end,
    {Status, {Reason, Target}, ProcState} =
        bpe_event:handle_event(Event, Target1, Proc),
    bpe:add_trace(ProcState,
                  [],
                  Target), %It will be better always use EventName instead of Target
    #sched{pointer = Pointer, state = ScheduledFlows} =
        bpe:sched_head(ProcState#process.id),
    bpe:add_sched(ProcState, Pointer, ScheduledFlows),
    kvs:append(ProcState, "/bpe/proc"),
    debug(ProcState,
          EventName,
          Targets,
          Target,
          Status,
          Reason),
    fix_reply({Status, {Reason, Target}, ProcState}).

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
        true -> [];
        _ ->
            bpe:add_trace(ProcState, [], Target),
            debug(ProcState, Curr, Targets, Target, Status, Reason)
    end,
    fix_reply({Status, {Reason, Target}, ProcState}).

fix_reply({stop, {Reason, Reply}, State}) ->
    {stop, Reason, Reply, State};
fix_reply(P) -> P.
fix_reply(continue, {reply, {{complete, _}, _}, State}, Acc, _) ->
  {noreply, State, {continue, Acc}};
fix_reply(continue, {reply, {complete, _}, State}, Acc, _) ->
  {noreply, State, {continue, Acc}};
fix_reply(continue, {reply, {complete, _}, State, {continue, Continue}}, Acc, _) ->
  {noreply, State, {continue, Acc ++ Continue}};
fix_reply(continue, {reply, {error, Message, _}, State}, _, _) ->
  {stop, Message, State};
fix_reply(continue, {reply, {unknown_task, Msg}, State}, _, _) ->
  {stop, {error, {unknown_task, Msg}}, State};
fix_reply(continue, {reply, _, State}, Acc, _) ->
  {noreply, State, {continue, Acc}};
fix_reply(continue, {stop, Reason, State}, _, _) ->
  {stop, Reason, State};
fix_reply(continue, {stop, Reason, _, State}, _, _) ->
  {stop, Reason, State};
fix_reply(continue, _, _, PrevState) ->
  {stop, {error, unknown_reply}, PrevState};
fix_reply(_, Reply, _, _) -> Reply.

convert_api_args(proc, [_ProcId]) -> {get};
convert_api_args(update, [_ProcId, State]) -> {set, State};
convert_api_args(assign, [_ProcId]) -> {ensure_mon};
convert_api_args(Fn, [_ | Args]) ->
  list_to_tuple([Fn | Args]).

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
handle_call({persist, State}, _, #process{id=Id} = _Proc) ->
    R = kvs:append(State, "/bpe/proc"),
    logger:notice("BPE: ~ts PERSIST RESULT ~p for ~p", [R, Id, self()]),
    {reply, State, State};
handle_call({next}, _, #process{id=Id} = Proc) ->
    logger:notice("BPE: ~ts NEXT START for ~p", [Id, self()]),
    R = try bpe:processFlow(Proc) catch
        _X:_Y:Z -> {reply, {error, 'next/1', Z}, Proc}
    end,
    logger:notice("BPE: ~ts NEXT RESULT ~p for ~p", [R, Id, self()]),
    R;
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
handle_call({event, Event}, _, Proc) ->
    try process_event(Event, Proc) catch
        _X:_Y:Z -> {reply, {error, 'event/2', Z}, Proc}
    end;
% BPMN 1.0 ПриватБанк
handle_call({complete}, _, Proc) ->
    try process_task([], Proc) catch
        _X:_Y:Z -> {reply, {error, 'complete/1', Z}, Proc}
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
handle_call(Command, _, Proc) ->
    {reply, {unknown, Command}, Proc}.

handle_continue([#continue{type=spawn, module=Module, fn=Fn, args=Args} | T], #process{} = Proc) ->
  spawn(fun() -> apply(Module, Fn, Args) end),
  {noreply, Proc, {continue, T}};
handle_continue([#continue{type=bpe, fn=Fn, args=Args} | T], #process{} = Proc) ->
    try fix_reply(continue, handle_call(convert_api_args(Fn, Args), [], Proc), T, Proc)
    catch
      _X:_Y:Z -> {stop, {error, Z}, Proc}
    end;
handle_continue([#continue{type=stop} | _], #process{} = Proc) ->
    {stop, normal, Proc};
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

handle_cast(Msg, State) ->
    logger:notice("BPE: Unknown API async: ~p.", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

handle_info({timer, ping},
            State = #process{timer = _Timer, id = _Id,
                             events = _Events, notifications = _Pid}) ->
    (application:get_env(bpe,
                         ping_discipline,
                         bpe_ping)):ping(State);
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

terminate(Reason, #process{id = Id}) ->
    logger:notice("BPE: ~ts terminate Reason: ~p",
                  [Id, Reason]),
    bpe:cache({process, Id}, undefined),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.
