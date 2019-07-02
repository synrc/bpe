-module(bpe_otp).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(application).
-behaviour(supervisor).
-export([start/2,stop/1,init/1]).

stop(_)      -> ok.
opt()        -> [ set, named_table, { keypos, 1 }, public ].
respawn()    -> spawn(fun () -> [ worker(I) || I <- kvs:feed("/bpe/proc")] end).
start(_,_)   -> [ bpe:reload(I) || I <- application:get_env(bpe,procmodules,[bpe_account,bpe]) ],
                syn:init(), kvs:join(), kvs:ensure(kvs:writer("/bpe/proc")),
                X = supervisor:start_link({local, ?MODULE}, ?MODULE, []), respawn(), X.
init([])     -> [ ets:new(T,opt()) || T <- [ processes ] ],
                { ok, { { one_for_one, 5, 10 }, [] } }.

worker(#process{id=Id}=P) ->
   io:format("Attempt to restart process ~p.~n",[Id]),
   case bpe:head(Id) of
        #hist{time = Time, task = Task} -> worker_do(calendar:time_difference(Time,calendar:local_time()),P);
        _ -> broken end;
worker(P) -> io:format("Unknown: ~p~n",[P]).

worker_do({Days,_Time},_) when Days >= 14 -> skip;
worker_do({_,_},P) -> bpe:start(P,[]).

