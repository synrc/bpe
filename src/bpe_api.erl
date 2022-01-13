-module(bpe_api).

-include_lib("bpe/include/bpe.hrl").
-include_lib("kvs/include/cursors.hrl").

-compile(export_all).

-define(TIMEOUT,
        application:get_env(bpe, timeout, 6000)).

load(Id) -> load(Id, []).

load(Id, Def) ->
    case application:get_env(kvs, dba, kvs_mnesia) of
        kvs_mnesia ->
            case kvs:get(process, Id) of
                {ok, P1} -> P1;
                {error, _Reason} -> Def
            end;
        kvs_rocks ->
            case kvs:get("/bpe/proc", Id) of
                {ok, P2} -> P2;
                {error, _Reason} -> Def
            end
    end.

add_hist(Key, Proc, Name, Task) ->
    Writer = kvs:writer(Key),
    kvs:append(#hist{id =
                        bpe:key({step, Writer#writer.count, Proc#process.id}),
                        name = Name, time = #ts{time = calendar:local_time()},
                        docs = Proc#process.docs, task = Task},
                Key).

start(#process{docs = Docs} = Proc, []) ->
    bpe:start(Proc, Docs, {[], #procRec{}});

start(Proc0, Options) ->
    bpe:start(Proc0, Options, {[], #procRec{}}).

proc(ProcId) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {get}, ?TIMEOUT).

update(ProcId, State) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {set, State}, ?TIMEOUT).

persist(ProcId, State) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId),
                    {persist, State},
                    ?TIMEOUT).

assign(ProcId) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {ensure_mon}, ?TIMEOUT).

complete(ProcId) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {complete}, ?TIMEOUT).

next(ProcId) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {next}, ?TIMEOUT).

complete(ProcId, Stage) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId),
                    {complete, Stage},
                    ?TIMEOUT).

next(ProcId, Stage) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {next, Stage}, ?TIMEOUT).

amend(ProcId, Form) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {amend, Form}, ?TIMEOUT).

discard(ProcId, Form) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {discard, Form}, ?TIMEOUT).

modify(ProcId, Form, Arg) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId),
                    {modify, Form, Arg},
                    ?TIMEOUT).

event(ProcId, Event) ->
    start(load(ProcId), []),
    gen_server:call(bpe:pid(ProcId), {event, Event}, ?TIMEOUT).

head(ProcId) ->
    Key = case application:get_env(kvs, dba, kvs_mnesia) of
                kvs_rocks -> bpe:key("/bpe/hist/", ProcId);
                kvs_mnesia -> hist
            end,
    case kvs:get(writer, bpe:key("/bpe/hist/", ProcId)) of
        {ok, #writer{count = C}} ->
            case kvs:get(Key, bpe:key({step, C - 1, ProcId})) of
                {ok, X} -> X;
                _ -> []
            end;
        _ -> []
    end.

hist(#step{proc = ProcId, id = N}) -> hist(ProcId, N);
hist(ProcId) -> kvs:all(bpe:key("/bpe/hist/", ProcId)).

hist(ProcId, N) ->
    Key = case application:get_env(kvs, dba, kvs_mnesia) of
                kvs_rocks -> bpe:key("/bpe/hist/", ProcId);
                kvs_mnesia -> hist
            end,
    case kvs:get(Key, bpe:key({step, N, ProcId})) of
        {ok, Res} -> Res;
        {error, _Reason} -> []
    end.

doc(R, Proc) ->
    {X, _} = bpe_env:find(env, Proc, R),
    X.

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
    Module:Fun(Proc);
check_flow_condition(#sequenceFlow{condition =
                                       {service, Fun, Module}},
                     Proc) ->
    Module:Fun(Proc).

