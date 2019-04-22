-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/kvs.hrl").

% BPMN 2.0 API


-record(task,         { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        roles=[] :: binary() }).
-record(userTask,     { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        roles=[] :: [] | binary() }).
-record(serviceTask,  { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        roles=[] :: [] | binary()}).
-record(receiveTask,  { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        roles=[] :: binary()}).
-record(messageEvent, { name=[] :: [] | atom() | string() | binary(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | {integer(),{integer(),integer(),integer()}} }).
-record(boundaryEvent,{ name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        payload=[] :: binary(),
                        timeout=[] :: {integer(),{integer(),integer(),integer()}},
                        timeDate=[] :: binary(),
                        timeDuration=[] :: binary(),
                        timeCycle=[] :: binary() }).
-record(timeoutEvent, { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | {integer(),{integer(),integer(),integer()}},
                        timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary() }).
-record(beginEvent ,  { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple())}).
-record(endEvent,     { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple())}).
-record(sequenceFlow, { source=[] :: [] | atom(),
                        target=[] :: [] | atom() | list(atom()) }).
-record(hist,         { ?ITERATOR(feed),
                        name=[] :: [] | binary(),
                        task=[] :: [] | atom() | {atom(),atom()},
                        docs=[] :: list(tuple()),
                        time=[] :: term() }).

-type tasks()  :: #task{} | #serviceTask{} | #userTask{} | #receiveTask{} | #beginEvent{} | #endEvent{}.
-type events() :: #messageEvent{} | #boundaryEvent{} | #timeoutEvent{}.

-record(process,      { ?ITERATOR(feed), name=[] :: [] | binary() | string() | atom(),
                        roles      = [] :: list(),
                        tasks      = [] :: list(tasks()),
                        events     = [] :: list(events()),
                        hist       = [] :: [] | term(),
                        flows      = [] :: list(#sequenceFlow{}),
                        rules      = [] :: [] | term(),
                        docs       = [] :: list(tuple()),
                        options    = [] :: term(),
                        task       = 'Init' :: [] | atom(),
                        timer      = [] :: [] | reference(),
                        notifications=[] :: [] | term(),
                        result     = [] :: [] | binary(),
                        started    = [] :: [] | {term(),term(),term()},
                        beginEvent = [] :: [] | atom(),
                        endEvent   = [] :: [] | atom()}).

% BPE API

-record('Comp', { id=[]   :: [] | integer() }).
-record('Proc', { id=[]   :: [] | integer() }).
-record('Load', { id=[]   :: [] | integer() }).
-record('Hist', { id=[]   :: [] | integer() }).
-record('Make', { proc=[] :: [] | #process{} | binary(), docs=[] :: [] | list(tuple()) }).
-record('Amen', { id=[]   :: [] | integer(), docs=[] :: [] | list(tuple()) }).

-endif.
