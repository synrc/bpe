-module(bpe_env).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-export([find/3,remove/3,append/3]).

find(kvs,Feed,Rec) -> find(Rec,kvs:all(Feed));
find(env,Proc,Rec) -> find(Rec,Proc#process.docs).
remove(kvs,Feed,Rec) -> {X,Y} = find(kvs,Feed,Rec), lists:map(fun(I)->kvs:delete(Feed,element(2,I))end,X);
remove(env,Proc,Rec) -> {X,Y} = find(env,Proc,Rec), Proc#process{docs = Y}.
append(kvs,Feed,Rec) -> kvs:append(Rec,Feed);
append(env,#process{id=Proc, docs = Docs} = P,Rec) ->
  case find(env,Proc,Rec) of
       {[],Env} -> P#process{docs = [Rec|Env]};
       {List,Env} -> P#process{docs = [Rec|List]++Env} end.

find(Rec,Feed) ->
  Zip = [ {X,Y} || {X,Y} <- lists:zip(lists:seq(1,size(Rec)),
                                      tuple_to_list(Rec)),Y/=[]],
  lists:partition(fun (R) ->
    lists:foldl(fun ({P,X},A) -> A andalso (element(P,R) == X)
    end, true, Zip)
  end, Feed).
