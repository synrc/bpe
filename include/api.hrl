% BPE API Type-Specification

% service

-spec load(integer()) -> #process{}.
-spec start(#process{},list()) -> {ok,integer()} | {error,integer()}.

% flow API

-spec process(integer()) -> #process{}.
-spec complete(integer()) -> {complete,any()}.
-spec complete(any(),integer()) -> {complete,any()}.
-spec amend(integer(),any()) -> {complete,any()}.
-spec event(integer(),any()) -> {complete,any()}.
-spec hist(integer()) -> list(#hist{}).

% find task or document in process

-spec task(Name::list(),#process{}) -> [tuple()] | tuple().
-spec doc(Record::tuple(),#process{}) -> [tuple()] | tuple().

% retrieve process field

-spec tasks(#process{}) -> list().
-spec docs(#process{}) -> list().
-spec events(#process{}) -> list().

