-module(sampleproc_process).
-author('Maxim Sokhatsky').
-include_lib("bpe/include/bpe.hrl").
-compile(export_all).

definition() -> 

    #process { name = 'Create Deposit Account',

        flows = [
            #sequenceFlow{source='Init',      target='Payment'},
            #sequenceFlow{source='Payment',   target='Signatory'},
            #sequenceFlow{source='Payment',   target='Process'},
            #sequenceFlow{source='Process',   target='Final'},
            #sequenceFlow{source='Signatory', target='Process'},
            #sequenceFlow{source='Signatory', target='Final'}
        ],

        tasks = [
            #userTask    { name='Init',      module = deposit }, 
            #userTask    { name='Signatory', module = deposit},
            #serviceTask { name='Payment',   module = deposit},
            #serviceTask { name='Process',   module = deposit},
            #endEvent    { name='Final'}
        ],

        beginEvent = 'Init',
        endEvent = 'Final',
        events = [
             #messageEvent{name="PaymentReceived"}
        ]
    }.
