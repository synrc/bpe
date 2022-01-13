-module(bpe_conv).

-include_lib("bpe/include/bpe.hrl").
-include_lib("kvs/include/cursors.hrl").

-export([load/1,
         load/2,
         add_hist/4,
         start/2,
         proc/1,
         update/2,
         persist/2,
         assign/1,
         complete/1,
         complete/2,
         next/1,
         next/2,
         amend/2,
         discard/2,
         modify/3,
         event/2,
         head/1,
         hist/1,
         hist/2,
         doc/2,
         check_flow_condition/2]).

-define(TIMEOUT,
        application:get_env(bpe, timeout, 6000)).

load(Id) -> load(Id, []).

load(Id, Def) ->
    case application:get_env(kvs, dba, kvs_mnesia) of
        kvs_mnesia ->
            case kvs:get(process, Id) of
                {ok, P1} -> deconv(P1);
                {error, _Reason} -> Def
            end;
        kvs_rocks ->
            case kvs:get("/bpe/proc", Id) of
                {ok, P2} -> deconv(P2);
                {error, _Reason} -> Def
            end
    end.

add_hist(Key, P, Name, Task) ->
    Writer = kvs:writer(Key),
    Proc = conv(P),
    kvs:append(#hist{id =
                        bpe:key({step, Writer#writer.count, Proc#process.id}),
                        name = Name, time = #ts{time = calendar:local_time()},
                        docs = Proc#process.docs, task = Task},
                Key).

start(#process{} = P, []) ->
    Proc = #process{docs = Docs} = conv(P),
    bpe:start(Proc, Docs, {[], #procRec{}});

start(Proc0, Options) ->
  bpe:start(conv(Proc0), lists:map(fun conv_doc/1, Options), {[], #procRec{}}).

proc(ProcId) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {get}, ?TIMEOUT).

update(ProcId, State) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {set, conv(State)}, ?TIMEOUT).

persist(ProcId, State) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId),
                    {persist, conv(State)},
                    ?TIMEOUT).

assign(ProcId) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {ensure_mon}, ?TIMEOUT).

complete(ProcId) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {complete}, ?TIMEOUT).

next(ProcId) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {next}, ?TIMEOUT).

complete(ProcId, Stage) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId),
                    {complete, Stage},
                    ?TIMEOUT).

next(ProcId, Stage) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {next, Stage}, ?TIMEOUT).

amend(ProcId, Form) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {amend, conv(Form)}, ?TIMEOUT).

discard(ProcId, Form) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {discard, Form}, ?TIMEOUT).

modify(ProcId, Form, Arg) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId),
                    {modify, Form, conv(Arg)},
                    ?TIMEOUT).

event(ProcId, Event) ->
    bpe_api:start(bpe_api:load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {event, Event}, ?TIMEOUT).

head(ProcId) ->
    Key = case application:get_env(kvs, dba, kvs_mnesia) of
                kvs_rocks -> bpe:key("/bpe/hist/", ProcId);
                kvs_mnesia -> hist
            end,
    case kvs:get(writer, bpe:key("/bpe/hist/", ProcId)) of
        {ok, #writer{count = C}} ->
            case kvs:get(Key, bpe:key({step, C - 1, ProcId})) of
                {ok, X} -> deconv(X);
                _ -> []
            end;
        _ -> []
    end.

hist(#step{proc = ProcId, id = N}) -> hist(ProcId, N);
hist(ProcId) -> lists:map(fun deconv/1, kvs:all(bpe:key("/bpe/hist/", ProcId))).

hist(ProcId, N) ->
    Key = case application:get_env(kvs, dba, kvs_mnesia) of
                kvs_rocks -> bpe:key("/bpe/hist/", ProcId);
                kvs_mnesia -> hist
            end,
    case kvs:get(Key, bpe:key({step, N, ProcId})) of
        {ok, Res} -> deconv(Res);
        {error, _Reason} -> []
    end.

doc(R, Proc) ->
    {X, _} = bpe_env:find(env, Proc, R),
    lists:map(fun deconv_doc/1, X).

check_flow_condition(#sequenceFlow{condition = []},
                     #process{}) ->
    true;
check_flow_condition(#sequenceFlow{condition =
                                       {compare, BpeDocParam, Field, ConstCheckAgainst}},
                     Proc) ->
    case doc(BpeDocParam, Proc) of
        [] ->
            bpe:add_error(Proc, "No such document", BpeDocParam),
            false;
        Docs when is_list(Docs) ->
            element(Field, hd(Docs)) == ConstCheckAgainst
    end;
check_flow_condition(#sequenceFlow{condition =
                                       {service, Fun}},
                     Proc = #process{module = Module}) ->
    Module:Fun(deconv(Proc));
check_flow_condition(#sequenceFlow{condition =
                                       {service, Fun, Module}},
                     Proc) ->
    Module:Fun(deconv(Proc)).

% Record -> {RecName, Id, Feed}
conv(Proc = #process{docs = Docs}) ->
  logger:notice("CONV PROС", []),
  Proc#process{docs = lists:map(fun conv_doc/1, Docs)};
conv(Hist = #hist{docs = Docs}) ->
  logger:notice("CONV HIST", []),
  Hist#hist{docs = lists:map(fun conv_doc/1, Docs)};
conv(X) -> X.

conv_doc(D) ->
  lists:foldl(fun (F, Acc) ->
    El = kvs:field(D, F),
    case is_rec(El) of
      true ->
        Feed = kvs:field(El, feed),
        case Feed of F0 when not is_binary(F0); F0 == <<"">> -> Acc; _ -> kvs:setfield(Acc, F, {element(1, El), element(2, El), Feed}) end;
      _ -> Acc
    end
  end, D, kvs:fields(element(1, D))).

is_rec(R) when is_tuple(R) ->
  R0 = element(1, R),
  is_atom(R0) andalso kvs:table(R0) /= false;
is_rec(_) -> false.

% {RecName, Id, Feed} -> Record
deconv(Proc = #process{docs = Docs}) ->
  logger:notice("DECONV PROC", []),
  Proc#process{docs = lists:map(fun deconv_doc/1, Docs)};
deconv(Hist = #hist{docs = Docs}) ->
  logger:notice("DECONV HIST", []),
  Hist#hist{docs = lists:map(fun deconv_doc/1, Docs)};
deconv(Proc) -> Proc.

deconv_doc(D) ->
  lists:foldl(fun (F, Acc) ->
    case kvs:field(D, F) of
      {Name, Id, Feed} when is_atom(Name) and is_binary(Feed) ->
        case kvs:get(Feed, Id) of {ok, V} -> kvs:setfield(Acc, F, V); _-> Acc end;
      _ -> Acc
    end
  end, D, kvs:fields(element(1, D))).
