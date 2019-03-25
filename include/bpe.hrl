-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("tour.hrl").

% BPMN 2.0 API

-record(task,         { name=[] :: [] | atom(),
                        roles=[] :: binary(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()) }).
-record(userTask,     { name=[] :: [] | atom(),
                        roles=[] :: binary(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()) }).
-record(serviceTask,  { name=[] :: [] | atom(),
                        roles=[] :: binary(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()) }).
-record(receiveTask,  { name=[] :: [] | atom(),
                        roles=[] :: binary(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple()) }).
-record(messageEvent, { name=[] :: [] | atom(),
                        payload=[] :: binary(),
                        timeout=[] :: {integer(),{integer(),integer(),integer()}},
                        module :: atom() }).
-record(boundaryEvent,{ name=[] :: [] | atom(),
                        payload=[] :: binary(),
                        timeout=[] :: {integer(),{integer(),integer(),integer()}},
                        timeDate=[] :: binary(),
                        timeDuration=[] :: binary(),
                        timeCycle=[] :: binary(),
                        module=[] :: [] | atom() }).
-record(timeoutEvent, { name=[] :: [] | atom(),
                        payload=[] :: [] | binary(),
                        timeout=[] :: [] | {integer(),{integer(),integer(),integer()}},
                        timeDate=[] :: [] | binary(),
                        timeDuration=[] :: [] | binary(),
                        timeCycle=[] :: [] | binary(),
                        module=[] :: [] | atom() }).
-record(beginEvent ,  { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple())}).
-record(endEvent,     { name=[] :: [] | atom(),
                        module=[] :: [] | atom(),
                        prompt=[] :: list(tuple())}).
-record(sequenceFlow, { source=[] :: [] | atom(),
                        target=[] :: [] | atom() }).
-record(hist,         { ?ITERATOR(feed),
                        name=[] :: [] | binary(),
                        task=[] :: atom(),
                        time=[] :: term() }).
-record(process,      { ?ITERATOR(feed), name=[] :: [] | binary(),
                        roles=[] :: list(),
                        tasks=[] :: list(#task{} | #serviceTask{} | #userTask{} | #receiveTask{}),
                        events=[] :: list(#messageEvent{} | #boundaryEvent{} | #timeoutEvent{}),
                        hist=[] :: [],
                        flows=[] :: list(#sequenceFlow{}),
                        rules=[] :: [],
                        docs=[] :: list(tuple()),
                        options=[] :: term(),
                        task=[] :: [] | atom(),
                        timer=[] :: [] | binary(),
                        notifications=[] :: [] | term(),
                        result=[] :: [] | binary(),
                        started=[] :: [] | binary(),
                        beginEvent=[] :: [] | atom(),
                        endEvent=[] :: [] | atom()}).

% BPE API

-record(complete,     { id=[] :: [] | integer() }).
-record(proc,         { id=[] :: [] | integer() }).
-record(histo,        { id=[] :: [] | integer() }).
-record(create,       { proc=[] :: [] | #process{} | binary(),
                        docs=[] :: [] | list(#join_application{} | #max_tour{} | #tour_list{}) }).
-record(amend,        { id=[] :: [] | integer(),
                        docs=[] :: [] | list(#join_application{} | #max_tour{} | #tour_list{}) }).

-endif.
