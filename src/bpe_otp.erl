-module(bpe_otp).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-behaviour(application).
-behaviour(supervisor).
-export([start/2,stop/1,init/1,respawn/0]).

stop(_)      -> ok.
opt()        -> [ set, named_table, { keypos, 1 }, public ].
respawn()    -> spawn(fun () -> [ worker(I) || I <- kvs:all("/bpe/proc")] end).
start(_,_)   -> [ bpe:reload(I) || I <- application:get_env(bpe,procmodules,[bpe_account,bpe]) ],
                logger:add_handlers(bpe),
                syn:init(), kvs:join(), kvs:ensure(kvs:writer("/bpe/proc")),
                supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([])     -> [ ets:new(T,opt()) || T <- [ processes ] ],
                { ok, { { one_for_one, 5, 10 }, [] } }.

worker(#process{id=Id}=P) ->
   case bpe:head(Id) of
        #hist{time = #ts{time=Time}} -> worker_do(calendar:time_difference(Time,calendar:local_time()),P);
        _ -> broken end;
worker(P) -> logger:notice("Unknown: ~p~n",[P]).

worker_do({Days,_Time},_) when Days >= 14 -> skip;
worker_do({_,_},P) -> bpe:start(P,[]).

