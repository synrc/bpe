-module(bpe_proc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

process_event(Event,Proc) ->
    Targets = bpe_task:targets(element(#messageEvent.name,Event),Proc),
    io:format("Event Targets: ~p",[Targets]),
    {Status,{Reason,Target},ProcState} = bpe_event:handle_event(Event,bpe_task:find_flow(Targets),Proc),

    kvs:add(#hist { id = kvs:next_id("hist",1),
                       feed_id = {hist,ProcState#process.id},
                       name = ProcState#process.name,
                       time = calendar:local_time(),
                       docs = ProcState#process.docs,
                       task = { event, element(#messageEvent.name,Event) }}),

    NewProcState = ProcState#process{task = Target},
    begin fix_reply({Status,{Reason,Target},NewProcState}) end.
%    ?LOG_INFO("Process ~p Flow Reply ~tp ",[Proc#process.id,{Status,{Reason,Target}}]),
%    kvs:put(transient(NewProcState)),
%    FlowReply.

process_task(Stage,Proc) -> process_task(Stage,Proc,false).
process_task(Stage,Proc,NoFlow) ->
    Curr = Proc#process.task,
    Term = [],
    Task = bpe:task(Curr,Proc),
    Targets = case NoFlow of
                   true -> noflow;
                   _ -> bpe_task:targets(Curr,Proc) end,

    io:format("Process ~p Task: ~p Targets: ~p",[Proc#process.id, Curr,Targets]),
    {Status,{Reason,Target},ProcState} = case {Targets,Proc#process.task,Stage} of
         {noflow,_,_} -> {reply,{complete,Curr},Proc};
         {[],Term,_}  -> bpe_task:already_finished(Proc);
         {[],Curr,_}  -> bpe_task:handle_task(Task,Curr,Curr,Proc);
         {[],_,_}     -> bpe_task:denied_flow(Curr,Proc);
         {List,_,[]}  -> bpe_task:handle_task(Task,Curr,bpe_task:find_flow(Stage,List),Proc);
         {List,_,_}   -> {reply,{complete,bpe_task:find_flow(Stage,List)},Proc} end,

    kvs:add(#hist { id = kvs:next_id("hist",1),
                       feed_id = {hist,ProcState#process.id},
                       name = ProcState#process.name,
                       time = calendar:local_time(),
                       docs = ProcState#process.docs,
                       task = {task, Curr} }),

    NewProcState = ProcState#process{task = Target},

    FlowReply = fix_reply({Status,{Reason,Target},NewProcState}),
    io:format("Process ~p Flow Reply ~tp ",[Proc#process.id,{Status,{Reason,Target}}]),
    kvs:put(transient(NewProcState)),
    FlowReply.

fix_reply({stop,{Reason,Reply},State}) -> {stop,Reason,Reply,State};
fix_reply(P) -> P.

handle_call({get},            _,Proc) -> { reply,Proc,Proc };
handle_call({run},            _,Proc) ->   run('Finish',Proc);
handle_call({until,Stage},    _,Proc) ->   run(Stage,Proc);
handle_call({event,Event},    _,Proc) ->   process_event(Event,Proc);
handle_call({start},          _,Proc) ->   process_task([],Proc);
handle_call({complete},       _,Proc) ->   process_task([],Proc);
handle_call({complete,Stage}, _,Proc) ->   process_task(Stage,Proc);
handle_call({amend,Form,true},_,Proc) ->   process_task([],Proc#process{docs=[Form]},true);
handle_call({amend,Form},     _,Proc) ->   process_task([],Proc#process{docs=[Form]});
handle_call({remove,Form},    _,Proc) ->   process_task([],Proc#process{docs=[
                                         { remove,element(1,Form),element(2,Form)}]},true);
handle_call(Command,_,Proc)           -> { reply,{unknown,Command},Proc }.

init(Process) ->
    io:format("Process ~p spawned ~p",[Process#process.id,self()]),
    Proc = case kvs:get(process,Process#process.id) of
         {ok,Exists} -> Exists;
         {error,_} -> Process end,
    Till = bpe:till(calendar:local_time(), kvs:config(bpe,ttl,24*60*60)),
    bpe:cache({process,Proc#process.id},self(),Till),
    [ bpe:reg({messageEvent,element(1,EventRec),Proc#process.id}) || EventRec <- bpe:events(Proc) ],
    {ok, Proc#process{timer=erlang:send_after(rand:uniform(10000),self(),{timer,ping})}}.

handle_cast(Msg, State) ->
    io:format("Unknown API async: ~p", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(500*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(bpe,ping,{0,0,5}).

handle_info({timer,ping}, State=#process{task=Task,timer=Timer,id=Id,events=Events,notifications=Pid}) ->
    case Timer of [] -> skip; _ -> erlang:cancel_timer(Timer) end,
    Wildcard = '*',

    Terminal= case lists:keytake(Wildcard,#messageEvent.name,Events) of
                   {value,Event,_} -> {Wildcard,element(1,Event),element(#messageEvent.timeout,Event)};
                             false -> {Wildcard,boundaryEvent,{5,ping()}} end,

    {Name,Record,{Days,Pattern}} = case lists:keytake(Task,#messageEvent.name,Events) of
                                       {value,Event2,_} -> {Task,element(1,Event2),element(#messageEvent.timeout,Event2)};
                                       false -> Terminal end,
    Time2 = calendar:local_time(),
    %io:format("Ping: ~p, Task ~p, Event ~p, Record ~p ~n", [Id,Task,Name,Record]),

    {DD,Diff} = case bpe:hist(Id,1) of
         [#hist{time=Time1}] -> calendar:time_difference(Time1,Time2);
          _ -> {immediate,timeout} end,

    case {{DD,Diff} < {Days,Pattern}, Record} of
        {true,_} -> {noreply,State#process{timer=timer_restart(ping())}};
        {false,timeoutEvent} ->
            io:format("BPE process ~p: next step by timeout. ~nTime Diff is ~p~n",[Id,{DD,Diff}]),
            case process_task([],State) of
                {reply,_,NewState} -> {noreply,NewState#process{timer=timer_restart(ping())}};
                {stop,normal,_,NewState} -> {stop,normal,NewState} end;
        {false,_} -> io:format("BPE process ~p: Closing Timeout. ~nTime Diff is ~p~n",[Id,{DD,Diff}]),
            case is_pid(Pid) of
                true -> Pid ! {direct,{bpe,terminate,{Name,{Days,Pattern}}}};
                false -> skip end,
            bpe:cache({process,Id},undefined),
            {stop,normal,State} end;

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State = #process{id=Id}) ->
    io:format(?MODULE, "connection closed, shutting down session:~p", [Msg]),
    bpe:cache({process,Id},undefined),
    {stop, normal, State};

handle_info(Info, State=#process{}) ->
    io:format("Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #process{id=Id}) ->
    io:format("Terminating session Id cache: ~p~n Reason: ~p", [Id,Reason]),
    spawn(fun() -> supervisor:delete_child(bpe_sup,Id) end),
    bpe:cache({process,Id},undefined),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

run(Task,Process) ->
    CurrentTask = Process#process.task,
    case bpe_proc:process_task([],Process,false) of
         {reply,{complete,Reached},NewProc}
           when Reached /= CurrentTask andalso Reached /= Task -> run(Task,NewProc);
         Else -> Else end.

transient(#process{docs=Docs}=Process) ->
    Process#process{docs=lists:filter(
        fun (X) -> not lists:member(element(1,X),
            application:get_env(bpe,transient,[])) end,Docs)}.
