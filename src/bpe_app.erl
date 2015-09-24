-module(bpe_app).
-behaviour(application).
-include("bpe.hrl").
-include_lib("kvs/include/feed.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Res = bpe_sup:start_link(),
    kvs:join(),
    Table = process,
    spawn(fun() -> case kvs:get(feed,Table) of
           {ok,Feed} -> kvs:fold(fun(A,Acc) -> {M,F} = application:get_env(bpe,process_worker,{?MODULE,worker}),
                                                M:F(A) end,[],
                        Table, Feed#feed.top,undefined, #iterator.prev,#kvs{mod=store_mnesia});
                  __ -> skip end end),
    Res.

stop(_State) -> ok.

worker(P) -> Ret = bpe:start(P,[]), wf:info(?MODULE,"Bpe Start: ~p~n",[Ret]).
