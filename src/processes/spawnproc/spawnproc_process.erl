-module(spawnproc_process).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-compile(export_all).

definition() ->

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
