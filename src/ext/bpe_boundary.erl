-module(bpe_boundary).
-include_lib("bpe/include/bpe.hrl").
-compile(export_all).

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(500*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(bpe,ping,{0,0,5}).

ping(State=#process{timer=Timer,id=Id,events=Events,notifications=Pid}) ->
    {_,Task} = bpe:current_task(State),
    case Timer of [] -> skip; _ -> erlang:cancel_timer(Timer) end,
%   search for '*' wildcard terminal event in process definition
    Terminal = case lists:keytake('*',#messageEvent.id,Events) of
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
            case bpe_proc:process_task([],State) of
                {reply,_,NewState} -> {noreply,NewState#process{timer=timer_restart(ping())}};
                {stop,normal,_,NewState} -> {stop,normal,NewState} end;
        {false,Record} -> logger:notice("BPE: ~p close ~p Timeout: ~p~n",[Id,Record,{DD,Diff}]),
            case is_pid(Pid) of
                true -> Pid ! {direct,{bpe,terminate,{Name,{Days,Pattern}}}};
                false -> skip end,
            bpe:cache({process,Id},undefined),
            {stop,normal,State} end.
