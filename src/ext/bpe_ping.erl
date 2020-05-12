-module(bpe_ping).
-include_lib("bpe/include/bpe.hrl").
-compile(export_all).

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(500*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(bpe,ping,{0,0,30}).
termination() -> application:get_env(bpe,boundary,{7,{0,0,0}}).
ping(State=#process{timer=Timer,id=Id,events=Events,notifications=Pid}) ->
    case Timer of [] -> skip; _ -> erlang:cancel_timer(Timer) end,
    case bpe:head(Id) of
        #hist{time=#ts{time=Time1}} ->
            {D,Diff} = calendar:time_difference(Time1,calendar:local_time()),
            io:format("BPE timer ~p: ~p~n",[Id,{D,Diff}]),
            case {D,Diff} > termination() of
                 true  -> {stop,normal,State};
                 false -> {noreply,State#process{timer=timer_restart(ping())}} end;
        _ -> skip
    end.
