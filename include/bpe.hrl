-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-include_lib("kvs/include/kvs.hrl").
-include("tour.hrl").

% BPMN 2.0 API

-record(task,         { name=[] :: [] | atom(), roles=[] :: list(), module=[] :: [] | atom() }).
-record(userTask,     { name=[] :: [] | atom(), roles=[] :: list(), module=[] :: [] | atom() }).
-record(serviceTask,  { name=[] :: [] | atom(), roles=[] :: list(), module=[] :: [] | atom() }).
-record(receiveTask,  { name=[] :: [] | atom(), roles=[] :: list(), module=[] :: [] | atom() }).
-record(messageEvent, { name=[] :: [] | atom(), payload=[] :: term(), timeout=[] :: term(), module :: atom() }).
-record(boundaryEvent,{ name=[] :: [] | atom(),
                        payload=[] :: term(),
                        timeout=[] :: term(),
                        timeDate=[] :: term(),
                        timeDuration=[] :: term(),
                        timeCycle=[] :: term(),
                        module=[] :: [] | atom() }).
-record(timeoutEvent, { name=[] :: [] | atom(),
                        payload=[] :: [] | term(),
                        timeout=[] :: [] | term(),
                        timeDate=[] :: [] | term(),
                        timeDuration=[] :: [] | term(),
                        timeCycle=[] :: [] | term(),
                        module=[] :: [] | term() }).
-record(beginEvent ,  { name=[] :: [] | atom(),
                        module=[] :: [] | atom()}).
-record(endEvent,     { name=[] :: [] | atom(),
                        module=[] :: [] | atom()}).
-record(sequenceFlow, { source=[] :: [] | atom(),
                        target=[] :: [] | atom() }).
-record(history,      { ?ITERATOR(feed),
                        name=[] :: [] | binary(),
                        task=[] :: atom(),
                        time=[] :: term() }).
-record(process,      { ?ITERATOR(feed), name=[] :: [] | binary(),
                        roles=[] :: list(),
                        tasks=[] :: list(#task{} | #serviceTask{} | #userTask{} | #receiveTask{}),
                        events=[] :: list(#messageEvent{} | #boundaryEvent{} | #timeoutEvent{}),
                        history=[] :: [],
                        flows=[] :: list(#sequenceFlow{}),
                        rules=[] :: [],
                        docs=[] :: list(tuple()),
                        options=[] :: term(),
                        task=[] :: [] | atom(),
                        timer=[] :: [] | term(),
                        notifications=[] :: [] | term(),
                        result=[] :: [] | term(),
                        started=[] :: [] | term(),
                        beginEvent=[] :: [] | atom(),
                        endEvent=[] :: [] | atom()}).

% BPE API

-record(complete,     { id=[] :: [] | integer() }).
-record(proc,         { id=[] :: [] | integer() }).
-record(hist,         { id=[] :: [] | integer() }).
-record(create,       { proc=[] :: [] | #process{} | binary(),
                        docs=[] :: [] | list(#join_application{} | #max_tour{} | #tour_list{}) }).
-record(amend,        { id=[] :: [] | integer(),
                        docs=[] :: [] | list(#join_application{} | #max_tour{} | #tour_list{}) }).

-endif.
