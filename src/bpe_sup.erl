-module(bpe_sup).
-author('Maxim Sokhatsky').
-description('BPE Supervisor').
-include("bpe.hrl").
-behaviour(supervisor).
-compile(export_all).
-export([start_link/0]).
-export([init/1]).

start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10000,
    MaxSecondsBetweenRestarts = 5000,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, { SupFlags, []} }.

