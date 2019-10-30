-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/cursors.hrl").

-define(DEFAULT_MODULE, application:get_env(bpe,default_module,bpe_xml)).

-define(TASK,           id=[] :: list(),
                        name=[] :: list() | binary(),
                        module=?DEFAULT_MODULE :: [] | atom(),
                        in=[] :: list(list()),
                        out=[] :: list(list()),
                        prompt=[] :: list(tuple()),
                        roles=[] :: list(atom()),
                        etc=[] :: list({term(),term()}) ).

-define(EVENT,          id=[] :: list() | atom(),
                        name=[] :: list() | binary(),
                        module=?DEFAULT_MODULE :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        etc=[] :: list({term(),term()}),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | #timeout{} ).

-define(CYCLIC,         timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary() ).

-record(timeout,      { spec= [] :: term() }).

-type condition() :: {compare,BpeDocParam :: {atom(),term()},Field :: integer(), ConstCheckAgainst :: term()}
                   | {service,atom()}.

-record(sequenceFlow, { id=[] :: list(),
                        name=[] :: list() | binary(),
                        condition=[] :: [] | condition() | binary(),
                        source=[] :: list(),
                        target=[] :: list(integer()) | list(list()) }).

-record(beginEvent ,  { ?TASK }).
-record(endEvent,     { ?TASK }).
-record(task,         { ?TASK }).
-record(userTask,     { ?TASK }).
-record(serviceTask,  { ?TASK }).
-record(receiveTask,  { ?TASK, reader=[] :: #reader{} }).
-record(sendTask,     { ?TASK, writer=[] :: #writer{} }).
-record(gateway,      { ?TASK, type= parallel :: gate() }).
-record(messageEvent, { ?EVENT }).
-record(messageBeginEvent, { ?EVENT }).
-record(boundaryEvent,{ ?EVENT, ?CYCLIC }).
-record(timeoutEvent, { ?EVENT, ?CYCLIC }).

-type tasks()  :: #task{} | #serviceTask{} | #userTask{} | #receiveTask{} | #sendTask{} | #beginEvent{} | #endEvent{}.
-type events() :: #messageEvent{} | #boundaryEvent{} | #timeoutEvent{}.
-type procId() :: [] | integer() | {atom(),any()}.
-type gate()   :: exclusive | parallel | inclusive | complex | event.

-record(ts,    { time= [] :: term() }).
-record(step,  { id = 0 :: integer(), proc = "" :: list() }).
-record(role,  { id = [] :: list(), name :: binary(), tasks = [] :: term() }).
-record(sched, { id = [] :: [] | #step{},
                 prev=[] :: [] | integer(),
                 next=[] :: [] | integer(),
                 pointer = -1 :: integer(),
                 state = [] :: list(list()) }).

-record(hist,         { id = [] :: [] | #step{},
                        prev=[] :: [] | integer(),
                        next=[] :: [] | integer(),
                        name=[] :: [] | binary() | list(),
                        task=[] :: [] | atom() | list() | #sequenceFlow{} | condition(),
                        docs=[] :: list(tuple()),
                        time=[] :: [] | #ts{} }).

-record(process,      { id = [] :: procId(),
                        prev=[] :: [] | integer(),
                        next=[] :: [] | integer(),
                        name=[] :: [] | binary() | string() | atom(),
                        feeds=[] :: list(),
                        roles      = [] :: term(),
                        tasks      = [] :: list(tasks()),
                        events     = [] :: list(events()),
                        flows      = [] :: list(#sequenceFlow{}),
                        docs       = [] :: list(tuple()),
                        options    = [] :: term(),
                        xml        = [] :: list(),
                        timer      = [] :: [] | reference(),
                        notifications=[] :: [] | term(),
                        result     = [] :: [] | binary(),
                        started    = [] :: [] | #ts{},
                        beginEvent = [] :: list() | atom(),
                        endEvent   = [] :: list() | atom() }).

-record(subProcess,   { name=[] :: [] | atom(),
                        diagram= #process{} :: #process{} }).

-record('Comp', { id=[]   :: [] | integer() }).
-record('Proc', { id=[]   :: [] | integer() }).
-record('Load', { id=[]   :: [] | integer() }).
-record('Hist', { id=[]   :: [] | integer() }).
-record('Make', { proc=[] :: [] | #process{} | binary(), docs=[] :: [] | list(tuple()) }).
-record('Amen', { id=[]   :: [] | integer(), docs=[] :: [] | list(tuple()) }).

-endif.
