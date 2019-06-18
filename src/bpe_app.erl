-module(bpe_app).
-behaviour(application).
-include("bpe.hrl").
-include_lib("kvx/include/cursors.hrl").
-export([start/2, stop/1, worker/1]).
-compile(export_all).

start(_StartType, _StartArgs) ->
    Res = bpe_sup:start_link(),
    Table = process,
    Res.

stop(_State) -> ok.

load_all() ->
   [ ?MODULE:worker(I) || I <- kvx:all(process)].

worker(#process{id=Id}=P) ->
   case bpe:hist(Id) of
        [H|_] -> worker_do(calendar:time_difference(H#hist.time,calendar:local_time()),P);
        _ -> bpe:start(P,[]) end.

worker_do({Days,_Time},_) when Days >= 14 -> skip;
worker_do({_Days,_Time},P) -> io:format("BPE Start: ~p~n",[bpe:start(P,[])]).
