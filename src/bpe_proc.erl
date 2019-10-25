-module(bpe_proc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

debug(Proc,Name,Targets,Target,Status,Reason) ->
    case application:get_env(bpe,debug,true) of
         true -> logger:notice("BPE: ~p [~s:~s] ~p/~p ~p~n",
                               [Proc#process.id,Name,Target,Status,Reason,Targets]);
         false -> skip end.

process_event(Event,Proc) ->
    EventName = element(#messageEvent.name,Event),
    Targets = bpe_task:targets(EventName,Proc),
    {Status,{Reason,Target},ProcState} = bpe_event:handle_event(Event,bpe_task:find_flow(Targets),Proc),
    bpe:trace(ProcState,[],calendar:local_time(),element(#messageEvent.name,Event)),
    debug(ProcState,EventName,Targets,Target,Status,Reason),
    fix_reply({Status,{Reason,Target},ProcState#process{task = Target}}).

process_task(Stage,Proc) -> process_task(Stage,Proc,false).
process_task(Stage,Proc,NoFlow) ->
    Curr = Proc#process.task,
    Task = bpe:step(Proc,Curr),
    Targets = case NoFlow of
                   true -> noflow;
                   _ -> bpe_task:targets(Curr,Proc) end,

    {Status,{Reason,Target},ProcState} =
       case {Targets,Curr,Stage} of
         {noflow,_,_} -> {reply,{complete,Curr},Proc};
         {[],[],_}    -> bpe_task:already_finished(Proc);
         {[],Curr,_}  -> bpe_task:handle_task(Task,Curr,Curr,Proc);
         {[],_,_}     -> bpe_task:denied_flow(Curr,Proc);
         {List,_,[]}  -> bpe_task:handle_task(Task,Curr,bpe_task:find_flow(Stage,List),Proc);
         {List,_,_}   -> {reply,{complete,bpe_task:find_flow(Stage,List)},Proc} end,

    case (Status == stop) orelse (NoFlow == true) of
       true -> [];
       _ -> bpe:add_trace(ProcState,[],Target),
            debug(ProcState,Curr,Targets,Target,Status,Reason) end,

    fix_reply({Status,{Reason,Target},ProcState#process{task = Target}}).

fix_reply({stop,{Reason,Reply},State}) -> {stop,Reason,Reply,State};
fix_reply(P) -> P.

% BPMN 2.0 Инфотех

handle_call({get},               _,Proc) -> { reply,Proc,Proc };
handle_call({next},              _,Proc) -> try bpe:processFlow(Proc) catch X:Y:Z -> {reply,{error,'next/1',Z},Proc} end;
handle_call({next,Stage},        _,Proc) -> try bpe:processFlow(Stage,Proc) catch X:Y:Z -> {reply,{error,'next/2',Z},Proc} end;
handle_call({amend,Form},        _,Proc) -> try bpe:processFlow(bpe_env:append(env,Proc,Form)) catch X:Y:Z -> {reply,{error,'amend/2',Z},Proc} end;
handle_call({discard,Form},      _,Proc) -> try bpe:processFlow(bpe_env:remove(env,Proc,Form)) catch X:Y:Z -> {reply,{error,'discard/2',Z},Proc} end;
handle_call({event,Event},       _,Proc) -> try process_event(Event,Proc) catch X:Y:Z -> {reply,{error,'event/2',Z},Proc} end;

% BPMN 1.0 ПриватБанк

handle_call({complete},          _,Proc) -> try process_task([],Proc) catch X:Y:Z -> {reply,{error,'complete/1',Z},Proc} end;
handle_call({complete,Stage},    _,Proc) -> try process_task(Stage,Proc) catch X:Y:Z -> {reply,{error,'complete/2',Z},Proc} end;
handle_call({modify,Form,append},_,Proc) -> try process_task([],bpe_env:append(env,Proc,Form),true) catch X:Y:Z -> {reply,{error,'append/2',Z},Proc} end;
handle_call({modify,Form,remove},_,Proc) -> try process_task([],bpe_env:remove(env,Proc,Form),true) catch X:Y:Z -> {reply,{error,'remove/2',Z},Proc} end;
handle_call(Command,_,Proc)              -> { reply,{unknown,Command},Proc }.

init(Process) ->
    Proc = bpe:load(Process#process.id,Process),
    logger:notice("BPE: ~p spawned as ~p~n",[Proc#process.id,self()]),
    Till = bpe:till(calendar:local_time(), application:get_env(bpe,ttl,24*60*60)),
    bpe:cache({process,Proc#process.id},self(),Till),
    [ bpe:reg({messageEvent,element(1,EventRec),Proc#process.id}) || EventRec <- bpe:events(Proc) ],
    {ok, Proc#process{timer=erlang:send_after(rand:uniform(10000),self(),{timer,ping})}}.

handle_cast(Msg, State) ->
    logger:notice("Unknown API async: ~p.~n", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(500*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(bpe,ping,{0,0,5}).

handle_info({timer,ping}, State=#process{task=Task,timer=Timer,id=Id,events=Events,notifications=Pid}) ->
    case Timer of [] -> skip; _ -> erlang:cancel_timer(Timer) end,
%   search for '*' wildcard terminal event in process definition
    Terminal = case lists:keytake('*',#messageEvent.name,Events) of
        {value,Event,_} -> {'*',element(1,Event),element(#messageEvent.timeout,Event)};
                  false -> {'*',none,#timeout{spec={none,none}}} end, % forever if none is set
%   search the events with the same name as current task, save event type and timeout
%   if no event found then use timeout information from terminal event
    {Name,Record,#timeout{spec={Days,Pattern}}} = case lists:keytake(Task,#messageEvent.name,Events) of
        {value,Event2,_} -> {Task,element(1,Event2),element(#messageEvent.timeout,Event2)};
                   false -> Terminal end,
%   calculate diff from past event
    {DD,Diff} = case bpe:head(Id) of
        #hist{time=#ts{time=Time1}} -> calendar:time_difference(Time1,calendar:local_time());
                        _ -> io:format("T~n"), {immediate,timeout} end,
%   io:format("Ping: ~p, Task: ~p Hist: ~p~n", [Id,Task,bpe:head(Id)]),
    case {{DD,Diff} < {Days,Pattern}, Record} of
        {_,none} -> {noreply,State#process{timer=timer_restart(ping())}};
        {true,_} -> {noreply,State#process{timer=timer_restart(ping())}};
        {false,timeoutEvent} -> % perform auto-complete on timeoutEvents
            logger:notice("BPE: ~p complete Timeout: ~p~n",[Id,{DD,Diff}]),
            case process_task([],State) of
                {reply,_,NewState} -> {noreply,NewState#process{timer=timer_restart(ping())}};
                {stop,normal,_,NewState} -> {stop,normal,NewState} end;
        {false,Record} -> logger:notice("BPE: ~p close ~p Timeout: ~p~n",[Id,Record,{DD,Diff}]),
            case is_pid(Pid) of
                true -> Pid ! {direct,{bpe,terminate,{Name,{Days,Pattern}}}};
                false -> skip end,
            bpe:cache({process,Id},undefined),
            {stop,normal,State} end;

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State = #process{id=Id}) ->
    logger:notice(?MODULE, "connection closed, shutting down session: ~p.~n", [Msg]),
    bpe:cache({process,Id},undefined),
    {stop, normal, State};

handle_info(Info, State=#process{}) ->
    logger:notice("Unrecognized info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, #process{id=Id}) ->
    logger:notice("BPE: ~p terminate Reason: ~p~n", [Id,Reason]),
    spawn(fun() -> supervisor:delete_child(bpe_otp,Id) end),
    bpe:cache({process,Id},undefined),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
