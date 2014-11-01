% BPE API Type-Specification

% service

-spec load(string()) -> #process{}.
-spec start(#process{},list()) -> {ok,pid()} | {error,any()}.

% flow API

-spec process(pid()) -> #process{}.
-spec complete(pid()) -> {complete,any()}.
-spec complete(any(),pid()) -> {complete,any()}.
-spec amend(pid(),any()) -> {complete,any()}.
-spec event(pid(),any()) -> {complete,any()}.
-spec history(pid()) -> list(#history{}).

% find task or document in process

-spec task(Name::list(),#process{tasks::list()}) -> [tuple()] | tuple().
-spec doc(Record::tuple(),#process{docs::list()}) -> [tuple()] | tuple().

% retrieve process field

-spec tasks(#process{}) -> list().
-spec docs(#process{}) -> list().
-spec events(#process{}) -> list().

% return helper

-spec val(Document::any(),
          Proc::#process{},
          fun((any(),#process{})->false|true),
          fun((any(),#process{})->any())) -> {reply,#process{}}
                                           | {reply,any(),#process{}}.
