-module(bpe_sup).
-include("bpe.hrl").
-behaviour(supervisor).
-compile(export_all).
-export([start_link/0]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_process(Process) ->
    wf:info(?MODULE,"BPE Start Process ~p: ",[Process]),
    Restart = transient,
    Shutdown = 200,
    ChildSpec = { Process#process.id,
                  { bpe_proc, start_link, [Process] },
                  Restart, Shutdown, worker, [bpe_proc] },
    supervisor:start_child(?MODULE,ChildSpec).

init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = permanent,
    Shutdown = 2000,
    Type = worker,

    {ok, { {one_for_one, 5, 10}, []} }.

