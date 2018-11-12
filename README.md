BPE: Business Process Engine
============================

[![Build Status](https://travis-ci.org/synrc/bpe.svg?branch=master)](https://travis-ci.org/synrc/bpe)

Overview
--------

BPE is Business Process Managament Application that brings Erlang for Enterprises.
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
that lately should be converted to Erlang terms. Internally BPE uses Erlang terms
workflow definition:

```erlang
bpe:start(spawnproc:def(),[]).
```

The workflow definition uses following persistent workflow model which is stored in KVS:

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

Full set of BPMN 2.0 fields could be obtained at [http://www.omg.org/spec/BPMN/2.0](http://www.omg.org/spec/BPMN/2.0) page 3-7.

Sample Session
--------------

```erlang
(bpe@127.0.0.1)1> rr(bpe).
[beginEvent,container,endEvent,history,id_seq,iterator,
 messageEvent,process,sequenceFlow,serviceTask,task,userTask]
(bpe@127.0.0.1)2> bpe:start(spawnproc:def(),[]).
bpe_proc:Process 39 spawned <0.12399.0>
{ok,<0.12399.0>}
(bpe@127.0.0.1)3> bpe:complete(39).
(bpe@127.0.0.1)4> bpe:complete(39).
(bpe@127.0.0.1)5> bpe:complete(39).
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

Process Instances
-----------------

Instantiation of process means creating persistent context of document flow.

```erlang
load(ProcName)
start(Proc,Docs)
amend(Proc,Docs)
complete(Proc)
history(ProcId)
task(Name,Proc)
doc(Name,Proc)
events(Proc)
tasks(Proc)
```

Using 'tasks' API you can fetch current documents attached to the given
process at particular stage. Using 'amend' API you can upload or
change document at current stage. 'push' API moves current
stage documents further by workflow.

Let us see how we could create initial 'Wire Transfer' transaction:

```erlang
> rr(bpe).
[ beginEvent,boundaryEvent,container,endEvent,history,id_seq,
  interval,iterator,kvs,log,messageEvent,operation,process,
  receiveTask,sequenceFlow,serviceTask,task,timeoutEvent,userTask ]

> rr(kvs).
[column,config,container,id_seq,interval,iterator,kvs,log,
 operation,query,schema,table,user,user2]

> Proc = bpe:load(39).

> bpe:tasks(Proc).
  [#userTask{name = 'Init',roles = [], module = spawnproc},
   #userTask{name = 'Signatory',roles = [], module = spawnproc},
   #serviceTask{name = 'Payment',roles = [], module = spawnproc},
   #serviceTask{name = 'Process',roles = [], module = spawnproc},
   #endEvent{name = 'Final',module = []}]

> bpe:docs(Proc).
  []

> bpe:amend(39,[{'WireTransfer',#user{id=1},#user{id=2}}]).

> bpe:docs(bpe:load(39)).
```

Dialyzer
--------

For check API with dialyzer build with `rebar compile` and run `make dialyze`.

Credits
-------

* Maxim Sokhatsky

OM A HUM
