-ifndef(BPE_HRL).
-define(BPE_HRL, true).
-include_lib("kvs/include/kvs.hrl").

-record(task,         { name, id, roles, module }).
-record(userTask,     { name, id, roles, module }).
-record(serviceTask,  { name, id, roles, module }).
-record(messageEvent, { name, id, payload }).
-record(beginEvent ,   { name, id }).
-record(endEvent,      { name, id }).
-record(sequenceFlow, { name, id, source, target }).
-record(history,      { ?ITERATOR(feed,true), name, task }).
-record(process,      { ?ITERATOR(feed,true), name,
                        roles=[], tasks=[], events=[], history=[], flows=[],
                        rules, docs=[],
                        task,
                        beginEvent, endEvent }).
-endif.
