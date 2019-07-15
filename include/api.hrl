% BPE API Type-Specification

% service

-spec load(string())            -> #process{}.
-spec start(#process{},list())  -> {ok,pid()} | {error,any()}.

% flow API

-spec process(pid())                -> #process{}.
-spec complete([] | integer())      -> {complete,any()}.
-spec complete(any(),pid())         -> {complete,any()}.
-spec amend([] | integer(),any())   -> {complete,any()}.
-spec event(pid(),any())            -> {complete,any()}.
-spec hist([] | integer())          -> list(#hist{}).

% find task or document in process

-spec task(Name::list(),#process{})     -> [tuple()] | tuple().
-spec doc(Record::tuple(),#process{})   -> [tuple()] | tuple().

% retrieve process field

-spec tasks(#process{})     -> list().
-spec docs(#process{})      -> list().
-spec events(#process{})    -> list().

% return helper

-spec val(Document::any(),
          Proc::#process{},
          fun((any(),#process{})  ->  false|true),
          fun((any(),#process{})  ->  any())) -> {reply,#process{}} | {reply,any(),#process{}}.
