% BPE API Type-Specification

% service

-spec load(term()) -> #process{}.
-spec start(#process{},list()) -> {ok,integer()} | {error,integer()}.

% flow API

-spec proc(integer()) -> #process{}.
-spec complete(term()) -> {complete,any()}.
-spec complete(any(),term()) -> {complete,any()}.
-spec amend(term(),any()) -> {complete,any()}.
-spec modify(term(),any()) -> {complete,any()}.
-spec event(term(),any()) -> {complete,any()}.
-spec hist(term()) -> list(#hist{}).

% find task or document in process

-spec step(term(),#process{}) -> [tuple()] | tuple().
-spec doc(tuple(),#process{}) -> [tuple()] | tuple().

% retrieve process field

-spec tasks(#process{}) -> list().
-spec docs(#process{}) -> list().
-spec events(#process{}) -> list().

