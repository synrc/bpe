-module(bpe_sup).
-author('Maxim Sokhatsky').
-description('BPE Supervisor').
-include("bpe.hrl").
-behaviour(supervisor).
-compile(export_all).
-export([start_link/0]).
-export([init/1]).

opt()        -> [ set, named_table, { keypos, 1 }, public ].
tables()     -> [ processes ].
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([])     -> [ ets:new(T,opt()) || T <- tables() ],
                { ok, { { one_for_one, 5, 10 }, [] } }.
