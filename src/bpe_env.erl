-module(bpe_env).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/cursors.hrl").
-compile([find/3,remove/3,append/3]).

find(kvs,Feed,Rec) -> find(Rec,kvs:all(Feed));
find(env,Proc,Rec) -> find(Rec,Proc#process.docs).
remove(kvs,Feed,Rec) -> kvs:delete(Feed,element(2,Rec));
remove(env,Proc,Rec) -> {X,Y} = find(Rec,Proc#process.docs), Proc#process{docs = Y}.
append(kvs,Feed,Rec) -> kvs:append(Rec,Feed);
append(env,#process{id=Proc, docs = Docs} = P,Rec) ->
  case find(env,Proc,Rec) of
       {[],Env} -> P#process{docs = [Rec|Env]};
       {List,Env} -> P#process{docs = [Rec|List]++Env} end.

find(Rec,Feed) ->
  {Tab,Key} = {element(1,Rec),element(2,Rec)},
  lists:partition(fun (R) ->
    lists:foldl(fun ({P,X},A) -> A andalso (element(P,R) == X)
    end, true, [{1,Tab},{2,Key}])
  end, Feed).
