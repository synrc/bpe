-module(bpe_otp).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(application).
-behaviour(supervisor).
-export([start/2,stop/1,init/1]).

stop(_)      -> ok.
opt()        -> [ set, named_table, { keypos, 1 }, public ].
respawn()    -> spawn(fun () -> [ worker(I) || I <- kvs:all(process)] end).
start(_,_)   -> [ bpe:reload(I) || I <- application:get_env(bpe,procmodules,[bpe_account]) ],
                syn:init(), kvs:join(), kvs:ensure(kvs:writer(process)),
                X = supervisor:start_link({local, ?MODULE}, ?MODULE, []), respawn(), X.
init([])     -> [ ets:new(T,opt()) || T <- [ processes ] ],
                { ok, { { one_for_one, 5, 10 }, [] } }.

worker(#process{id=Id}=P) ->
   io:format("Attempt to restart ~p.~n",[Id]),
   Writer = kvs:writer({hist,Id}),
   case bpe:hist(Id, Writer#writer.count-1) of
        #hist{time = Time} -> worker_do(calendar:time_difference(Time,calendar:local_time()),P);
        _ -> broken end.

worker_do({Days,_Time},_) when Days >= 14 -> skip;
worker_do({_,_},P) -> bpe:start(P,[]).

