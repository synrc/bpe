-ifndef(BPE_HRL).
-define(BPE_HRL, true).
-include_lib("kvs/include/kvs.hrl").

-record(task,         { name, id, roles }).
-record(userTask,     { name, id, roles }).
-record(serviceTask,  { name, id, roles, module }).
-record(messageEvent, { name, id, payload }).
-record(startEvent,   { name, id }).
-record(endEvent,     { name, id }).
-record(sequenceFlow, { name, id, source, target }).
-record(history,      { ?ITERATOR(feed,true), name, task }).
-record(process,      { ?ITERATOR(feed,true), name,
                        roles=[], tasks=[], events=[], history=[], flows=[],
                        rules, docs=[],
                        task,
                        beginTask, endTask }).
-endif.
