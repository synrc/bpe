-module(bpe_proc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([plist_setkey/4]).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

process_event(Event,Proc) ->
    Targets = bpe_task:targets(Event#messageEvent.name,Proc),
    io:format("Event Targets: ~p",[Targets]),
    {Status,{Reason,Target},ProcState} = bpe_event:handle_event(Event,bpe_task:find_flow(Targets),Proc),
    NewProcState = ProcState#process{task = Target},
    FlowReply = fix_reply({Status,{Reason,Target},NewProcState}),
    wf:info(?MODULE,"Process ~p Flow Reply ~p ",[Proc#process.id,{Status,{Reason,Target}}]),
    kvs:put(NewProcState),
    FlowReply.

process_flow(Stage,Proc) -> process_flow(Stage,Proc,false).
process_flow(Stage,Proc,NoFlow) ->
    Curr = Proc#process.task,
    Term = [],
    Task = bpe:task(Curr,Proc),
    Targets = case NoFlow of
                   true -> noflow;
                   _ -> bpe_task:targets(Curr,Proc) end,
    wf:info(?MODULE,"Process ~p Task: ~p Targets: ~p",[Proc#process.id, Curr,Targets]),
    {Status,{Reason,Target},ProcState} = case {Targets,Proc#process.task,Stage} of
         {noflow,_,_} -> {reply,{complete,Curr},Proc};
         {[],Term,_}  -> bpe_task:already_finished(Proc);
         {[],Curr,_}  -> bpe_task:handle_task(Task,Curr,Curr,Proc);
         {[],_,_}     -> bpe_task:denied_flow(Curr,Proc);
         {List,_,[]}  -> bpe_task:handle_task(Task,Curr,bpe_task:find_flow(Stage,List),Proc);
         {List,_,_}   -> {reply,{complete,bpe_task:find_flow(Stage,List)},Proc} end,

    kvs:add(#history { id = kvs:next_id("history",1),
                       feed_id = {history,ProcState#process.id},
                       name = ProcState#process.name,
                       time = calendar:now_to_datetime(now()),
                       task = {task, Curr} }),

    NewProcState = ProcState#process{task = Target},

    FlowReply = fix_reply({Status,{Reason,Target},NewProcState}),
    wf:info(?MODULE,"Process ~p Flow Reply ~p ",[Proc#process.id,{Status,{Reason,Target}}]),
    kvs:put(NewProcState),
    FlowReply.

fix_reply({stop,{Reason,Reply},State}) -> {stop,Reason,Reply,State};
fix_reply(P) -> P.

handle_call({get},_,Proc)              -> { reply,Proc,Proc };
handle_call({start},_,Proc)            ->   process_flow([],Proc);
handle_call({complete},_,Proc)         ->   process_flow([],Proc);
handle_call({complete,Stage},_,Proc)   ->   process_flow(Stage,Proc);
handle_call({event,Event},_,Proc)      ->   process_event(Event,Proc);
handle_call({amend,Form,true},_,Proc)
                   when is_list(Form)  ->   process_flow([],set_rec_in_proc(Proc,Form),true);
handle_call({amend,Form,true},_,Proc)  ->   process_flow([],Proc#process{docs=plist_setkey(element(1,Form),1,Proc#process.docs,Form)},true);
handle_call({amend,Form},_,Proc)
                   when is_list(Form)  ->   process_flow([],set_rec_in_proc(Proc,Form));
handle_call({amend,Form},_,Proc)       ->   process_flow([],Proc#process{docs=plist_setkey(element(1,Form),1,Proc#process.docs,Form)});
handle_call(Command,_,Proc)            -> { reply,{unknown,Command},Proc }.

init(Process) ->
    wf:info(?MODULE,"Process ~p spawned ~p",[Process#process.id,self()]),
    Proc = case kvs:get(process,Process#process.id) of
         {ok,Exists} -> Exists;
         {error,_} -> Process end,
    [ wf:reg({messageEvent,Name,Proc#process.id}) || {Name,_} <- bpe:events(Proc) ],
    {ok, Proc}.

handle_cast(Msg, State) ->
    wf:info(?MODULE,"Unknown API async: ~p", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

handle_info({'DOWN', _MonitorRef, _Type, _Object, _Info} = Msg, State = #process{}) ->
    wf:info(?MODULE, "connection closed, shutting down session:~p", [Msg]),
    {stop, normal, State};

handle_info({timer,ping}, State=#process{timer=Timer,id=Id}) ->
    case Timer of undefined -> skip; _ -> erlang:cancel_timer(Timer) end,
    wf:info(?MODULE,"Ping: ~p", [Id]),
    [H|T] = bpe:history(Id),
    Time2 = calendar:now_to_datetime(now()),
    case H of #history{time=Time1} ->
              case calendar:time_difference(Time1,Time2) of
                   {0,{0,0,_}} ->
                        erlang:send_after(5000,self(),{timer,ping}),
                        {noreply, State};
                   _ -> {stop,normal,State} end;
         _ -> {noreply, State} end;

handle_info(Info, State=#process{}) ->
    wf:info(?MODULE,"Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(_Reason, #process{id=Id}) ->
    wf:info(?MODULE,"Terminating session: ~p", [Id]),
    spawn(fun()->supervisor:delete_child(bpe_sup,Id) end),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

plist_setkey(Name,Pos,List,New) ->
    case lists:keyfind(Name,Pos,List) of
        false -> [New|List];
        _Element -> lists:keyreplace(Name,Pos,List,New) end.

set_rec_in_proc(Proc, []) -> Proc;
set_rec_in_proc(Proc, [H|T]) ->
    ProcNew = Proc#process{ docs=plist_setkey(element(1,H),1,Proc#process.docs,H)},
    set_rec_in_proc(ProcNew, T).
