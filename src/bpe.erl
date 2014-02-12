-module(bpe).
-include("bpe.hrl").
-compile(export_all).

% Instance Management

start(Process, Tasks) -> ok.
join(Self, Id) -> ok.
amend(Id, Tasks) -> ok.
step(Stage) -> ok.
finish(Id) -> ok.
history(Id) -> ok.
tasks(Id) -> ok.

% Process Schema

create(Process) -> ok.
add_stage(Process,Stage) -> ok.
add_role(Process,Role) -> ok.
assign_role(Stage,Role) -> ok.
delete(Process) -> ok.

test() ->
    #process{name='Wire Transfer',stages=[
        #stage{name='Request',
               roles=[department],
               transitions=['Approve'],
               action={wt,request,['WireTransferReq',
                                  {'Invoice',optional},
                                  {'Voucher',optional}]}},
        #stage{name='Approve',
               roles=[disbursement],
               transitions=['Request','Process','Payroll'],
               action={wt,approve,['Signature']}},

        #stage{name='Payroll',
               roles=[payroll],
               transitions=['Process'],
               action={wt,payroll,['TaxIssue']}},

        #stage{name='Process',
               roles=[disbursement],
               transitions=['Notify'],
               action={wt,process,['WireTransaction']}},

        #stage{name='Notify',
               roles=[disbursement],
               transitions=[],
               action={wt,notify,['Log']}}

    ],rules=[]}.