Business Process Engine
=======================

Overview
--------

BPE is Synrc Cloud Stack Erlang Application that bring Erlang for for Enterprise.
It provides infrastructure for Workflow Definitions, Process Orchestration,
Rule Based Production Systems and Distributed Storage.

Workflow Engine
===============

Workflow Engine -- is an Erlang/OTP application which handles process definitions,
process instances, and provides very clean API for Workplaces.

Process Schema
--------------

Before using Process Engine you need to define the set of business process
workflows of your enterprise. This could be done via Erlang terms or some DSL
that lately converted to Erlang terms. Internally BPE uses Eralng terms
workflow definition:

```erlang
   bpe:start(#process{name="Order11",
       flows=[
           #sequenceFlow{source="begin",target="end2"},
           #sequenceFlow{source="end2",target="end"}],
       tasks=[
           #userTask{name="begin"},
           #userTask{name="end2"},
           #endEvent{name="end"}],
       task="begin",beginEvent="begin",endEvent="end"},[]).

```

The worklow definiton uses following persistent workflow model which is stored in KVS:

```erlang
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
```

Sample Session
--------------

```erlang
(bpe@127.0.0.1)1> kvs:join().
ok
(bpe@127.0.0.1)1> rr(bpe).
[beginEvent,container,endEvent,history,id_seq,iterator,
 messageEvent,process,sequenceFlow,serviceTask,task,userTask]
(bpe@127.0.0.1)2> bpe:start(#process{name="Order11",
         flows=[#sequenceFlow{source="begin",target="end2"},
                #sequenceFlow{source="end2",target="end"}],
         tasks=[#userTask{name="begin"},
                #userTask{name="end2"},
                #endEvent{name="end"}],
         task="begin",beginEvent="begin",endEvent="end"},[]).
bpe_proc:Process 39 spawned <0.12399.0>
{ok,<0.12399.0>}
(bpe@127.0.0.1)3> gen_server:call(pid(0,12399,0),{complete}).
(bpe@127.0.0.1)4> gen_server:call(pid(0,12399,0),{complete}).
(bpe@127.0.0.1)5> gen_server:call(pid(0,12399,0),{complete}).
(bpe@127.0.0.1)5> bpe:history(39).
[#history{id = 28,version = undefined,container = feed,
          feed_id = {history,39},
          prev = 27,next = undefined,feeds = [],guard = true,
          etc = undefined,name = "Order11",
          task = {task,"end"}},
 #history{id = 27,version = undefined,container = feed,
          feed_id = {history,39},
          prev = 26,next = 28,feeds = [],guard = true,etc = undefined,
          name = "Order11",
          task = {task,"end2"}},
 #history{id = 26,version = undefined,container = feed,
          feed_id = {history,39},
          prev = undefined,next = 27,feeds = [],guard = true,
          etc = undefined,name = "Order11",
          task = {task,"begin"}}]
```

Internally the API of process definition looks like:

```erlang
load(File)
save(File)
create(Process)
add_stage(Process,Stage)
remove_stage(Process,Stage)
delete(Process)
definitions()
```
Process Instances
-----------------

Instantiation of process means creating persistent context of document flow.

```erlang
start(Process, Tasks)
join(Self, Id)
amend(Id, Tasks)
push(Id)
finish(Id)
history(Id)
tasks(Id)
list()
```

Using 'tasks' API you fetch current documents attached to the given
process at particular stage. Using 'amend' API you could upload or
change document at current stage. 'push' API move current
stage documents further by workflow.

Let us see how we could initial 'Wire Transfer' transaction:

```erlang
bpe:load("WireTransfer"),
Id = bpe:start('WireTransfer',[]),
[] = bpe:tasks(Id), % current set is empty
Tasks = [ #'WireTranswerReq'{
            beneficiary = #agent{ bank="SBININBB380",
                                  name="Namdak Tonpa",
                                  account="305820317"},
            subsidiary = #agent { bank="BKTRUS33",
                                  name="Maxim Sokhatsky",
                                  account="804250223"}}],

bpe:amend(Id,Tasks),
bpe:push(Id),
```

Credits
-------

* Maxim Sokhatsky

OM A HUM
