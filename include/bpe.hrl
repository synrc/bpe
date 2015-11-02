-include_lib("kvs/include/kvs.hrl").

-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-record(task,         { name, roles=[], module }).
-record(userTask,     { name, roles=[], module }).
-record(serviceTask,  { name, roles=[], module }).
-record(receiveTask,  { name, roles=[], module }).
-record(messageEvent, { name, payload=[], timeout=[], module }).
-record(boundaryEvent,{ name, payload=[], timeout=[], timeDate=[], timeDuration=[], timeCycle=[], module }).
-record(timeoutEvent, { name, payload=[], timeout=[], timeDate=[], timeDuration=[], timeCycle=[], module }).
-record(beginEvent ,  { name, module }).
-record(endEvent,     { name, module }).
-record(sequenceFlow, { source, target }).
-record(history,      { ?ITERATOR(feed,true), name, task, time }).
-record(process,      { ?ITERATOR(feed,true), name,
                        roles=[], tasks=[], events=[], history=[], flows=[],
                        rules, docs=[], options=[],
                        task, timer, notifications, result,
                        started, beginEvent, endEvent }).
-endif.
