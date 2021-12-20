-ifndef(CONSTRUCTOR_HRL).
-define(CONSTRUCTOR_HRL, true).

-record(bpeActivity, { id   = [] :: list(),
                       name = [] :: binary(),
                       type = [] :: list() % beginEvent, endEvent, task, gateway
                     }).

-record(bpeEvent, { id      = [] :: list(),
                    name    = [] :: binary(),
                    type    = [] :: list(),
                    timeout = [] :: binary() | list() | tuple()}).

-record(bpeFlow, { id,
                   name,
                   type, % beginEvent, endEvent, task, gateway
                   source,
                   target,
                   condition % erlang code
                }).

-record(bpeProc, { id         = [] :: list(),
                   name       = [] :: binary(),
                   activities = [] :: list(),
                   flows      = [] :: list(),
                   events     = [] :: list(),
                   roles      = [] :: list()
                }).

-record(bpeField, { id=[], name=[], pos=[], title=[], layout=[], visible=true, disabled=false,
                      vector=false, index=[], bind=[], format="~w", desc=[], wide=normal,
                      type=binary, labelClass=label, fieldClass=field, boxClass=box, multiple=false,
                      required=false, module=[], access=[], tooltips=[], options=[], min=0, distinct=false,
                      max=1000000, length=10, validation=[], hidden=false, default=[]}).

-endif.
