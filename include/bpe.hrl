-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/cursors.hrl").

-record(timeout,      { spec= [] :: term() }).

-record(sequenceFlow, { name=[] :: term(),
                        condition=[] :: term(),
                        source=[] :: [] | atom(),
                        target=[] :: [] | atom() | list(atom()) }).

-record(task,         { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        in=[] :: list(#sequenceFlow{}),
                        out=[] :: list(#sequenceFlow{}),
                        prompt=[] :: list(tuple()),
                        roles=[] :: [] | binary(),
                        etc=[] :: list({term(),term()}) }).

-record(userTask,     { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        in=[] :: list(#sequenceFlow{}),
                        out=[] :: list(#sequenceFlow{}),
                        prompt=[] :: list(tuple()),
                        roles=[] :: [] | binary(),
                        etc=[] :: list({term(),term()})  }).

-record(serviceTask,  { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        roles=[] :: [] | binary(),
                        in=[] :: list(#sequenceFlow{}),
                        out=[] :: list(#sequenceFlow{}),
                        etc=[] :: list({term(),term()}) }).

-record(receiveTask,  { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        in=[] :: list(#sequenceFlow{}),
                        out=[] :: list(#sequenceFlow{}),
                        prompt=[] :: list(tuple()),
                        reader=[] :: #reader{},
                        roles=[] :: [] | binary(),
                        etc=[] :: list({term(),term()}) }).

-record(sendTask,     { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        in=[] :: list(#sequenceFlow{}),
                        out=[] :: list(#sequenceFlow{}),
                        prompt=[] :: list(tuple()),
                        writer=[] :: #writer{},
                        roles=[] :: [] | binary(),
                        etc=[] :: list({term(),term()}) }).

-record(messageEvent, { name=[] :: [] | atom() | string() | binary(),
                        module=bpe_xml :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        etc=[] :: list({term(),term()}),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | #timeout{}}).

-record(boundaryEvent,{ name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        etc=[] :: list({term(),term()}),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | #timeout{},
                        timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary() }).

-record(timeoutEvent, { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        etc=[] :: list({term(),term()}),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | #timeout{},
                        timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary() }).

-record(beginEvent ,  { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        in=[] :: list(#sequenceFlow{}),
                        out=[] :: list(#sequenceFlow{}),
                        prompt=[] :: list(tuple()),
                        etc=[] :: list({term(),term()}) }).

-record(endEvent,     { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        in=[] :: list(#sequenceFlow{}),
                        out=[] :: list(#sequenceFlow{}),
                        prompt=[] :: list(tuple()),
                        etc=[] :: list({term(),term()}) }).





-type tasks()  :: #task{} | #serviceTask{} | #userTask{} | #receiveTask{} | #beginEvent{} | #endEvent{}.
-type events() :: #messageEvent{} | #boundaryEvent{} | #timeoutEvent{}.
-type procId() :: [] | integer() | {atom(),any()}.
-type gate()   :: exclusive | parallel | inclusive | complex | event.

-record(ts,           { time= [] :: term() }).
-record(step,         { id = 0 :: integer(), proc = "" :: list() }).

-record(sched, { id = [] :: [] | #step{},
                 pointer = -1 :: integer(),
                 state = [] :: list(#sequenceFlow{})  }).

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

-record(gateway,      { name=[] :: [] | atom(),
                        module=bpe_xml :: [] | atom(),
                        type= parallel :: gate(),
                        filler=[] :: [],
                        inputs=[] :: atom() | list(atom()),
                        outputs=[] :: atom() | list(atom()) }).

-record(subProcess,   { name=[] :: [] | atom(),
                        diagram= #process{} :: #process{} }).

-record('Comp', { id=[]   :: [] | integer() }).
-record('Proc', { id=[]   :: [] | integer() }).
-record('Load', { id=[]   :: [] | integer() }).
-record('Hist', { id=[]   :: [] | integer() }).
-record('Make', { proc=[] :: [] | #process{} | binary(), docs=[] :: [] | list(tuple()) }).
-record('Amen', { id=[]   :: [] | integer(), docs=[] :: [] | list(tuple()) }).

-endif.
