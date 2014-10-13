-module(bpe_proc).
-include("bpe.hrl").
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

start_link(Parameters) -> gen_server:start_link(?MODULE, Parameters, []).

process_flow(Proc,CurrentTaskName) ->
    Targets = [ Target || #sequenceFlow{source=Source,target=Target} <- Proc#process.flows,
            Source==CurrentTaskName],
    case Targets of
        [Target] -> 

             kvs:add(history, #history { id   = Proc#process.id,
                                         name = Proc#process.name,
                                         task = Proc#process.task }),

             {reply,{completed,Target},Proc#process{task=Target}};
        _ -> {reply,{ambiguous,Targets},Proc} end.

handle_call({start},From,#process{}=Proc)     -> process_flow(Proc,Proc#process.beginTask);
handle_call({complete},From,#process{}=Proc)  -> process_flow(Proc,Proc#process.task);
handle_call({amend,Docs},From,#process{}=Proc)->
     {reply,{modified,Proc#process{docs=Docs}}}.

init(#process{} =Process) ->
    wf:info(?MODULE,"Process gen_server INIT ~p",[Process]),
    kvs:put(Process),
    [ wf:reg({Name,Process#process.id}) || {Name,_} <- bpe:events(Process) ],
    {ok, Process}.

handle_cast(Msg, State) ->
    wf:info(?MODULE,"Unknown API async: ~p", [Msg]),
    {stop, {error, {unknown_cast, Msg}}, State}.

handle_info({'DOWN', MonitorRef, _Type, _Object, _Info} = Msg, State = #process{}) ->
    wf:info(?MODULE, "connection closed, shutting down session:~p", [Msg]),
    {stop, normal, State};

handle_info(Info, State=#process{}) ->
    wf:info(?MODULE,"Unrecognized info: ~p", [Info]),
    {noreply, State}.

terminate(Reason, #process{}) ->
    wf:info(?MODULE,"Terminating session: ~p", [Reason]),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
