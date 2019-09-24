BPE: Business Process Engine
============================

[![Build Status](https://travis-ci.org/synrc/bpe.svg?branch=master)](https://travis-ci.org/synrc/bpe)
[![Hex pm](http://img.shields.io/hexpm/v/bpe.svg?style=flat)](https://hex.pm/packages/bpe)

Overview
--------

BPE is a Business Process Engine that brings BPMN to Erlang and Erlang to Enterprises.
It provides infrastructure for Workflow Definitions, Process Orchestration,
Rule Based Production Systems and Distributed Storage.

Processes
---------

Processes are main entities of BPE, they map one-to-one to Erlang processes.
Basically, BPE process is an algorithm or function, that is executed entirely in the
context of Erlang process. The arguments for such algorithms are:
values from infinite streams (KVS chains);
values from Erlang messages being sent to BPE process.

```erlang
-record(hist, { id, next, prev, name, task, docs, time }).
-record(process, { id, next, prev, roles, tasks, events, hist, flows, rules, docs,
                   options, task, timer, notify, result, beginEvent, endEvent }).
```

During execution of the process, all steps are being written to the persistent storage,
by which execution logic is restorable and reproducable. The process definition is actually
diagram or graph where points represented by `task` and egdes by `sequenceFlow`.

Tasks and Flows
---------------

The step itself is represented as `task` (points). The transition between steps is
represented as `sequenceFlow` (edges). 

```erlang
-record(task, { name, id, roles, module }).
-record(userTask, { name, id, roles, module }).
-record(manualTask, { name, id, roles, module }).
-record(serviceTask, { name, id, roles, module }).
-record(receiveTask, { name, id, roles, module }).
-record(sendTask, { name, id, roles, module }).
-record(subProcess,  { name, id, roles, module }).
```

The history record of process execution is
represented as `hist` and captures the `sequenceFlow` information.

```erlang
-record(sequenceFlow, { name, id, source, target }).
-record(messageFlow, { name, id, source, target }).
```

Events
------

While Tasks are deterministic, where you're getting a new task from previous one,
the Events are non-deterministic, where you could get a new task by external
event from the system to the process.

```erlang
-record(beginEvent, { name, id }).
-record(endEvent, { name, id }).
-record(timeoutEvent, { name, id }).
-record(messageEvent, { name, id }).
-record(boundaryEvent, { name, id }).
```

Gateways
--------

Gateways represent multiplexors and demultiplexors which cause non-linear trace and multiple
current states as leaves of execution graph.

```erlang
-record(gateway, { name, type, inputs, outputs }).
```

Full set of BPMN 2.0 fields could be obtained
at [http://www.omg.org/spec/BPMN/2.0](http://www.omg.org/spec/BPMN/2.0), page 3-7.

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
