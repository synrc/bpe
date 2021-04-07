-module(bpe_env).
-author('Maxim Sokhatsky').
-include_lib("bpe/include/bpe.hrl").
-export([append/3,find/2,find/3,remove/3]).

append(kvs,Feed,Rec) ->
    kvs:append(Rec,Feed);

append(env,P,[]) -> P;
append(env,P,[Rec|Tail]) -> append(env,append(env,P,Rec),Tail);

append(env,#process{id=_Proc, docs = _Docs} = P,Rec) when is_tuple(Rec) ->
    Feed = "/bpe/proc",
    S = case find(env,P,Rec) of
        {[],Rest} -> P#process{docs = [Rec|Rest]};
        {Found,Rest} -> P#process{docs = [Rec|Found]++Rest} end,
    kvs:append(S, Feed),
    X = S#process{ modified = #ts{time = calendar:local_time()} },
    kvs:put(X),
    X.

find(Rec,Feed) ->
    lists:partition(fun (R) ->
      lists:foldl(fun ({P,X},A) -> A andalso (element(P,R) == X) end, true,
            [ {X,Y} || {X,Y} <- lists:zip(lists:seq(1,size(Rec)), tuple_to_list(Rec)),Y/=[]])
    end, Feed).

find(kvs,Feed,Rec) ->
    find(Rec,kvs:all(Feed));

find(env,Proc,Rec) ->
    find(Rec,Proc#process.docs).

remove(env,P,[]) -> P;
remove(env,P,[Rec|Tail]) -> remove(env,remove(env,P,Rec),Tail);

remove(kvs,Feed,Rec) when is_tuple(Rec) ->
    {X,_Y} = find(kvs,Feed,Rec),
    lists:map(fun(I) -> kvs:delete(Feed,element(2,I)) end, X);

remove(env,Proc,Rec) when is_tuple(Rec) ->
    {_X,Y} = find(env,Proc,Rec),
    S=Proc#process{docs=Y, modified = #ts{ time = calendar:local_time()}},
    kvs:append(S,"/bpe/proc"),
    kvs:put(S),
    S.
