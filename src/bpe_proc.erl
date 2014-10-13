-module(bpe_proc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

process_flow(Proc) ->

    Curr = Proc#process.task,
    Term = [],
    Task = bpe:task(Curr,Proc),
    Targets = bpe_task:targets(Curr,Proc),

    wf:info(?MODULE,"Process ~p Task: ~p Targets: ~p",[Proc#process.id, Curr,Targets]),

    {Status,{Reason,Target},ProcState} = case {Targets,Proc#process.task} of
         {[],Term} -> bpe_task:already_finished(Proc);
         {[],Curr} -> bpe_task:handle_task(Task,Curr,Term,Proc);
         {[],_}    -> bpe_task:denied_flow(Curr,Proc);
         {List,_}  -> bpe_task:handle_task(Task,Curr,bpe_task:find_flow(List),Proc) end,

    kvs:add(#history { id = kvs:next_id("history",1),
                       feed_id = {history,ProcState#process.id},
                       name = ProcState#process.name,
                       task = {task, Curr} }),

    NewProcState = ProcState#process{task = Target},

    FlowReply = fix_reply({Status,{Reason,Target},NewProcState}),
    wf:info(?MODULE,"Process ~p Flow Reply ~p ",[Proc#process.id,FlowReply]),
    kvs:put(NewProcState),
    FlowReply.

fix_reply({stop,{Reason,Reply},State}) -> {stop,Reason,Reply,State};
fix_reply(P) -> P.

handle_call({start},From,#process{}=Proc)     -> process_flow(Proc);
handle_call({complete},From,#process{}=Proc)  -> process_flow(Proc);
handle_call({amend,Docs},From,#process{}=Proc)-> {reply,{modified},Proc#process{docs=Docs}};
handle_call(Command,From,#process{}=Proc)-> {reply,{unknown,Command},Proc}.

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

handle_info({'DOWN', MonitorRef, _Type, _Object, _Info} = Msg, State = #process{}) ->
    wf:info(?MODULE, "connection closed, shutting down session:~p", [Msg]),
    {stop, normal, State};

handle_info({amend,Docs}, State=#process{}) ->
    wf:info(?MODULE,"Apply Frontend Documents: ~p", [Docs]),
    {noreply, State};

handle_info({messageEvent,Name,ProcId,Message}, ProcState=#process{}) ->
    wf:info(?MODULE,"messageEvent: ~p", [Message]),

    kvs:add(#history { id   = kvs:next_id("history",1),
                       feed_id = {history,ProcState#process.id},
                       name = ProcState#process.name,
                       task = {event,Name} }),

    bpe:complete(ProcState),

    {noreply, ProcState};

handle_info(Info, State=#process{}) ->
    wf:info(?MODULE,"Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #process{id=Id}) ->
    wf:info(?MODULE,"Terminating session: ~p", [Id]),
    spawn(fun()->supervisor:delete_child(bpe_sup,Id) end),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
