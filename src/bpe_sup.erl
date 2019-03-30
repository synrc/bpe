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
start_link() -> case application:get_env(bpe,nostand,true) of
                   false -> case application:get_env(bpe,kids,false) of
                              true -> cowboy:start_clear(http, [{port, port()}],
                                      #{env => #{dispatch => n2o_cowboy2:points()} });
                             false -> cowboy:start_tls(http, [{port, port()},
                                       {certfile, code:priv_dir(bpe)++"/ssl/fullchain.pem"},
                                       {keyfile, code:priv_dir(bpe)++"/ssl/privkey.pem"},
                                       {cacertfile, code:priv_dir(bpe)++"/ssl/fullchain.pem"}],
                                       #{env => #{dispatch => points()} }) end,
                            io:format("Cowboy Started~n");
                   true -> skip end,
                 supervisor:start_link({local, ?MODULE}, ?MODULE, []).
points()     -> cowboy_router:compile([{'_', [
                {"/ws/[...]", n2o_cowboy2, []},
                {"/app/[...]", cowboy_static, {dir, code:priv_dir(bpe)++"/static", []}} ]}]).
init([])     -> [ ets:new(T,opt()) || T <- tables() ],
                { ok, { { one_for_one, 5, 10 }, [] } }.
