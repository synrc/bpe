-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/cursors.hrl").

-define(DEFAULT_MODULE, application:get_env(bpe,default_module,bpe_xml)).

-define(TASK,           name=[] :: [] | atom(),
                        module=?DEFAULT_MODULE :: [] | atom(),
                        in=[] :: list(atom()),
                        out=[] :: list(atom()),
                        prompt=[] :: list(tuple()),
                        roles=[] :: list(atom()),
                        etc=[] :: list({term(),term()}) ).

-define(EVENT,          name=[] :: [] | atom(),
                        module=?DEFAULT_MODULE :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        etc=[] :: list({term(),term()}),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | #timeout{} ).

-define(CYCLIC,         timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary() ).

-record(timeout,      { spec= [] :: term() }).

-record(sequenceFlow, { name=[] :: term(),
                        condition=[] :: term(),
                        source=[] :: [] | atom(),
                        target=[] :: [] | atom() | list(atom()) }).

-record(beginEvent ,  { ?TASK }).
-record(endEvent,     { ?TASK }).
-record(task,         { ?TASK }).
-record(userTask,     { ?TASK }).
-record(serviceTask,  { ?TASK }).
-record(receiveTask,  { ?TASK, reader=[] :: #reader{} }).
-record(sendTask,     { ?TASK, writer=[] :: #writer{} }).
-record(gateway,      { ?TASK, type=parallel :: gate(), switch_doc=[] :: atom() }).
-record(messageEvent, { ?EVENT }).
-record(boundaryEvent,{ ?EVENT, ?CYCLIC }).
-record(timeoutEvent, { ?EVENT, ?CYCLIC }).

-type tasks()  :: #task{} | #serviceTask{} | #userTask{} | #receiveTask{} | #sendTask{} | #beginEvent{} | #endEvent{}.
-type events() :: #messageEvent{} | #boundaryEvent{} | #timeoutEvent{}.
-type procId() :: [] | integer() | {atom(),any()}.
-type gate()   :: exclusive | parallel | inclusive | complex | event.

-record(ts,           { time= [] :: term() }).
-record(step,         { id = 0 :: integer(), proc = "" :: list() }).

-record(sched, { id = [] :: [] | #step{},
                 pointer = -1 :: integer(),
                 state = [] :: list(atom()) }).

-record(hist,         { id = [] :: [] | #step{},
                        container=feed :: [] | atom(),
                        feed_id=[] :: any(),
                        prev=[] :: [] | integer(),
                        next=[] :: [] | integer(),
                        name=[] :: [] | binary(),
                        task=[] :: [] | atom(),
                        docs=[] :: list(tuple()),
                        time=[] :: [] | #ts{} }).

-record(process,      { id = [] :: procId(),
                        container=feed :: [] | atom(),
                        feed_id=[] :: [] | atom() | term(),
                        prev=[] :: [] | integer(),
                        next=[] :: [] | integer(),
                        name=[] :: [] | binary() | string() | atom(),
                        feeds=[] :: list(),
                        roles      = [] :: list(),
                        tasks      = [] :: list(tasks()),
                        events     = [] :: list(events()),
                        hist       = [] :: [] | term(),
                        flows      = [] :: list(#sequenceFlow{}),
                        rules      = [] :: [] | term(),
                        docs       = [] :: list(tuple()),
                        options    = [] :: term(),
                        task       = 'Created' :: [] | atom(),
                        timer      = [] :: [] | reference(),
                        notifications=[] :: [] | term(),
                        result     = [] :: [] | binary(),
                        started    = [] :: [] | #ts{},
                        beginEvent = [] :: [] | atom(),
                        endEvent   = [] :: [] | atom()}).

-record(subProcess,   { name=[] :: [] | atom(),
                        diagram= #process{} :: #process{} }).

-record('Comp', { id=[]   :: [] | integer() }).
-record('Proc', { id=[]   :: [] | integer() }).
-record('Load', { id=[]   :: [] | integer() }).
-record('Hist', { id=[]   :: [] | integer() }).
-record('Make', { proc=[] :: [] | #process{} | binary(), docs=[] :: [] | list(tuple()) }).
-record('Amen', { id=[]   :: [] | integer(), docs=[] :: [] | list(tuple()) }).

-endif.
