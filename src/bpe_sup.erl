-module(bpe_sup).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-behaviour(supervisor).
-compile(export_all).
-export([start_link/0]).
-export([init/1]).

opt()        -> [ set, named_table, { keypos, 1 }, public ].
tables()     -> [ processes ].
port()       -> application:get_env(n2o,port,8003).
start_link() -> syn:init(), kvx:join(), kvx:ensure(kvx:writer(process)),
                supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([])     -> [ ets:new(T,opt()) || T <- tables() ],
                { ok, { { one_for_one, 5, 10 }, [] } }.
