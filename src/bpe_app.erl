-module(bpe_app).
-behaviour(application).
-include("bpe.hrl").
-export([start/2, stop/1]).

start(_StartType, _StartArgs) -> 
    Res = bpe_sup:start_link(),
    [ {ok,_}=bpe:start(P,[]) || P <- kvs:all(process), P#process.task =:= 'Payment' ],
    Res.

stop(_State) -> ok.
