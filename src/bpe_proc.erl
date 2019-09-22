-module(bpe_proc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-compile(export_all).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

process_event(Event,Proc) ->
    EventName = element(#messageEvent.name,Event),
    Targets = bpe_task:targets(EventName,Proc),

    {Status,{Reason,Target},ProcState} = bpe_event:handle_event(Event,bpe_task:find_flow(Targets),Proc),

    Key = "/bpe/hist/" ++ ProcState#process.id,
    Writer = kvs:writer(Key),

    % the reason we need compund keys here for id field
    % is that in mnesia backend all hist entries are stored in one table
    % so step position is not enough. For RocksDB you can use just writer.count.

    kvs:append(#hist{ id = {Writer#writer.count,ProcState#process.id},
                    name = [],
                    time = calendar:local_time(),
                    docs = ProcState#process.docs,
                    task = { event, element(#messageEvent.name,Event) }}, Key),

    io:format("Process: ~p Event: ~p Targets: ~p~n",[Proc#process.id,EventName,Targets]),
    io:format("Target: ~p Status: ~p Reason: ~p",[Target,Status,Reason]),

    fix_reply({Status,{Reason,Target},ProcState#process{task = Target}}).

process_task(Stage,Proc) -> process_task(Stage,Proc,false).
process_task(Stage,Proc,NoFlow) ->
    Curr = Proc#process.task,
    Term = [],
    Task = bpe:step(Curr,Proc),
    Targets = case NoFlow of
                   true -> noflow;
                   _ -> bpe_task:targets(Curr,Proc) end,

    {Status,{Reason,Target},ProcState} = case {Targets,Proc#process.task,Stage} of
         {noflow,_,_} -> {reply,{complete,Curr},Proc};
         {[],Term,_}  -> bpe_task:already_finished(Proc);
         {[],Curr,_}  -> bpe_task:handle_task(Task,Curr,Curr,Proc);
         {[],_,_}     -> bpe_task:denied_flow(Curr,Proc);
         {List,_,[]}  -> bpe_task:handle_task(Task,Curr,bpe_task:find_flow(Stage,List),Proc);
         {List,_,_}   -> {reply,{complete,bpe_task:find_flow(Stage,List)},Proc} end,

    case (Status == stop) orelse (NoFlow == true) of true -> []; _ ->

    Key = "/bpe/hist/" ++ProcState#process.id,
    Writer = kvs:writer(Key),
    kvs:append(#hist{   id = {Writer#writer.count,ProcState#process.id},
                      name = [],
                      time = calendar:local_time(),
                      docs = ProcState#process.docs,
                      task = {task, Target} }, Key),

    io:format("Process: ~p Task: ~p Targets: ~p ~n",[Proc#process.id,Curr,Targets]),
    io:format("Target: ~p Status: ~p Reason: ~p~n",[Target,Status,Reason])

    end,

    fix_reply({Status,{Reason,Target},ProcState#process{task = Target}}).

fix_reply({stop,{Reason,Reply},State}) -> {stop,Reason,Reply,State};
fix_reply(P) -> P.

handle_call({get},            _,Proc) -> { reply,Proc,Proc };
handle_call({run},            _,Proc) ->   run('Final',Proc);
handle_call({until,Stage},    _,Proc) ->   run(Stage,Proc);
handle_call({event,Event},    _,Proc) ->   process_event(Event,Proc);
handle_call({start},          _,Proc) ->   process_task([],Proc);
handle_call({complete},       _,Proc) ->   process_task([],Proc);
handle_call({complete,Stage}, _,Proc) ->   process_task(Stage,Proc);
handle_call({modify,Form},    _,Proc) ->   process_task([],Proc#process{docs=[Form]},true);
handle_call({amend,Form},     _,Proc) ->   process_task([],Proc#process{docs=[Form]});
handle_call({remove,Form},    _,Proc) ->   process_task([],Proc#process{docs=[
                                         { remove,element(1,Form),element(2,Form)}]},true);
handle_call(Command,_,Proc)           -> { reply,{unknown,Command},Proc }.

init(Process) ->
    Proc = bpe:load(Process#process.id,Process),
    io:format("Process ~p spawned as ~p.~n",[Proc#process.id,self()]),
    Till = bpe:till(calendar:local_time(), application:get_env(bpe,ttl,24*60*60)),
    bpe:cache({process,Proc#process.id},self(),Till),
    [ bpe:reg({messageEvent,element(1,EventRec),Proc#process.id}) || EventRec <- bpe:events(Proc) ],
    {ok, Proc#process{timer=erlang:send_after(rand:uniform(10000),self(),{timer,ping})}}.

handle_cast(Msg, State) ->
    io:format("Unknown API async: ~p.~n", [Msg]),
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

    {DD,Diff} = case bpe:head(Id) of
                     #hist{time=Time1} -> calendar:time_difference(Time1,Time2);
                                     _ -> {immediate,timeout} end,

%   io:format("Ping: ~p, Task: ~p Hist: ~p~n", [Id,Task,Hist]),

    case {{DD,Diff} < {Days,Pattern}, Record} of
        {true,_} -> {noreply,State#process{timer=timer_restart(ping())}};
        {false,timeoutEvent} ->
            io:format("BPE ~p: next step by timeout.~nDiff: ~p.~n",[Id,{DD,Diff}]),
            case process_task([],State) of
                {reply,_,NewState} -> {noreply,NewState#process{timer=timer_restart(ping())}};
                {stop,normal,_,NewState} -> {stop,normal,NewState} end;
        {false,_} -> io:format("BPE ~p: closing Timeout.~nDiff: ~p.~n",[Id,{DD,Diff}]),
            case is_pid(Pid) of
                true -> Pid ! {direct,{bpe,terminate,{Name,{Days,Pattern}}}};
                false -> skip end,
            bpe:cache({process,Id},undefined),
            {stop,normal,State} end;

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State = #process{id=Id}) ->
    io:format(?MODULE, "connection closed, shutting down session: ~p.~n", [Msg]),
    bpe:cache({process,Id},undefined),
    {stop, normal, State};

handle_info(Info, State=#process{}) ->
    io:format("Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #process{id=Id}) ->
    io:format("Terminating session Id cache: ~p~n Reason: ~p", [Id,Reason]),
    spawn(fun() -> supervisor:delete_child(bpe_otp,Id) end),
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
