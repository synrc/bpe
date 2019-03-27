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
start_link() ->  case application:get_env(bpe,nostand,true) of
                   false -> cowboy:start_clear(http, [{port, 8003}],
                            #{ env => #{dispatch => n2o_cowboy2:points()} }),
                            io:format("Cowboy Started~n");
                   true -> skip end,
                 supervisor:start_link({local, ?MODULE}, ?MODULE, []).
points()     -> cowboy_router:compile([{'_', [
                {"/ws/[...]", n2o_cowboy2, []},
                {"/app/[...]", cowboy_static, {dir, code:priv_dir(bpe)++"/static", []}} ]}]).
init([])     -> [ ets:new(T,opt()) || T <- tables() ],
                { ok, { { one_for_one, 5, 10 }, [] } }.
