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

{process,'Wire Transfer',
    {stages,[
        {stage, 'Request',[department], ['Approve'],
                {wt,request,['WireTransferReq',
                             {'Invoice',optional},
                             {'Voucher',optional}]}},
        {stage, 'Approve',[disbursement], ['Request','Process','Payroll'],
                {wt,approve,['Signature']}},
        {stage, 'Payroll',[payroll], ['Process']
                {wt,payroll,['TaxIssue']}},
        {stage, 'Process',[disbursement], ['Notify'],
                {wt,process,['WireTransaction']}},
        {stage, 'Notify',[disbursement], [],
                {wt,notify,['Log']}}]},
    []
}.
```

The worklow definiton uses following persistent workflow model which is stored in KVS:

```erlang
-record(process,{name,stages,rules}).
-record(stage,{name,role,transitions,action}).
```

This workflow defition consists of two parts: the workflow discriptive structure
and the compiled workflow rules in Erlang module wt.

```erlang
-module(wt). % Wire Transfer module
-export([request,approve,payroll,process,notify]).
```

Internally the API of process definition looks like:

```erlang
load(File)
save(File)
create(Process)
add_stage(Process,Stage)
remove_stage(Process,Stage)
delete(Process)
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
