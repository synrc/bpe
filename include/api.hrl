% BPE API Type-Specification

-spec load(term()) -> #process{}.
-spec assign(term()) -> #process{}.
-spec persist(term(), #process{}) -> #process{}.
-spec start(#process{},list()) -> {ok,integer()} | {error,integer()}.
-spec proc(term()) -> #process{}.
-spec complete(term()) -> {complete,any()}.
-spec next(term()) -> {complete,any()}.
-spec complete(any(),term()) -> {complete,any()}.
-spec amend(term(),any()) -> {complete,any()}.
-spec discard(term(),any()) -> {complete,any()}.
-spec modify(term(),any(),append|remove) -> {complete,any()}.
-spec messageEvent(term(),any()) -> {complete,any()}.
-spec hist(term()) -> list(#hist{}) | #hist{}.

% find task or document in process

-spec step(#process{},term()) -> [tuple()] | tuple().
-spec tasks(#process{}) -> list().
-spec docs(#process{}) -> list().
-spec doc(tuple(),#process{}) -> list().
-spec events(#process{}) -> list().

