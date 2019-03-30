-module(spawnproc).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include_lib("kvs/include/user.hrl").
-compile(export_all).

def() ->

    #process { name = 'Deposit Account',

        flows = [
            #sequenceFlow{source='Init',      target='Payment'},
            #sequenceFlow{source='Payment',   target='Signatory'},
            #sequenceFlow{source='Payment',   target='Process'},
            #sequenceFlow{source='Process',   target='Final'},
            #sequenceFlow{source='Signatory', target='Process'},
            #sequenceFlow{source='Signatory', target='Final'}
        ],

        tasks = [
            #userTask    { name='Init',      module = spawnproc },
            #userTask    { name='Signatory', module = spawnproc },
            #serviceTask { name='Payment',   module = spawnproc },
            #serviceTask { name='Process',   module = spawnproc },
            #endEvent    { name='Final'}
        ],

        beginEvent = 'Init',
        endEvent = 'Final',
        events = [
%             #boundaryEvent{ name = '*', timeout={0,{0,30,0}} },
             #messageEvent{name="PaymentReceived"}
        ]
    }.

action({request,'Init'}, Proc) ->
    io:format("ACT Deposit Init~n"),
    {reply,Proc};

action({request,'Payment'}, Proc) ->
    Payment = bpe:doc({payment_notification},Proc),
    io:format("ACT Deposit Payment~n"),
    case is_tuple(Payment) of
         true  -> {reply,'Process',Proc};
         false -> {reply,'Signatory',Proc} end;

action({request,'Signatory'}, Proc) ->
    {reply,'Process',Proc};

action({request,'Process'}, Proc) ->

    io:format("ACT Deposit Process~n"),

    Account = #user{id=Proc#process.id},
    kvs:add(Account),
    io:format("ACT Create Account ~p ~n",[Account]),

    {reply,Proc};

action({request,'Final'}, Proc) ->
    io:format("ACT Deposit Finale~n"),
    % SQL backlog
    {reply,Proc}.

