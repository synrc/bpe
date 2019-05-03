-module(bpe_app).
-behaviour(application).
-include("bpe.hrl").
-include_lib("kvs/include/feed.hrl").
-export([start/2, stop/1, worker/1]).

start(_StartType, _StartArgs) ->
    Res = bpe_sup:start_link(),
    kvs:join(),
    Table = process,
    spawn(fun() -> case kvs:get(feed,Table) of
          {ok,Feed} -> kvs:fold(fun(A,_) ->
                           {M,F} = application:get_env(bpe,process_worker,{?MODULE,worker}),
                           M:F(A) end,[],
                       Table, Feed#feed.top,undefined, #iterator.prev,#kvs{mod=store_mnesia});
                 __ -> skip end end),
    Res.

stop(_State) -> ok.

worker(#process{id=Id}=P) -> [H|_] = bpe:hist(Id),
   worker_do(calendar:time_difference(H#hist.time,calendar:local_time()),P).

worker_do({Days,_Time},_) when Days >= 14 -> skip;
worker_do({_Days,_Time},P) -> io:format("BPE Start: ~p~n",[bpe:start(P,[])]).
