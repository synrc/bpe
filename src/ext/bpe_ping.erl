-module(bpe_ping).
-include_lib("bpe/include/bpe.hrl").
-compile(export_all).

timer_restart(Diff) -> {X,Y,Z} = Diff, erlang:send_after(500*(Z+60*Y+60*60*X),self(),{timer,ping}).
ping() -> application:get_env(bpe,ping,{0,0,30}).
termination() -> application:get_env(bpe,boundary,{7,{0,0,0}}).
ping(State=#process{timer=Timer,id=Id,modified = #ts{time=Time2}, events=Events,notifications=Pid}) ->
    case Timer of [] -> skip; _ -> erlang:cancel_timer(Timer) end,
    case bpe:head(Id) of
        #hist{time=#ts{time=Time1}} ->
            {D1,Diff1} = calendar:time_difference(Time1,calendar:local_time()),
            {D2,Diff2} = calendar:time_difference(Time2,calendar:local_time()),
            % io:format("BPE timer ~p: ~p~n",[Id,[{D1,Diff1},{D2,Diff2}]]),
            case {D1,Diff1} > termination() orelse {D2,Diff2} > termination() of
                 true  -> {stop,normal,State};
                 false -> {noreply,State#process{timer=timer_restart(ping())}} end;
        _ -> skip
    end.
