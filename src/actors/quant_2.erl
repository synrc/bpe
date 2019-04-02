-module(quant_2).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("doc.hrl").
-compile(export_all).

def() ->

    #process { name = 'Track Client',

        flows = [
            #sequenceFlow{source='Init', target='NDA'},
            #sequenceFlow{source='NDA',  target=['MSA','Final']},
            #sequenceFlow{source='MSA',  target=['Team','Final']},
            #sequenceFlow{source='Team', target='Validation'},
            #sequenceFlow{source='Validation', target=['Team','Succ']},
            #sequenceFlow{source='Succ', target='Final'}
        ],

        tasks = [
            #userTask    { name='Init',       module = quant_2 },
            #userTask    { name='NDA',        module = quant_2 },
            #serviceTask { name='MSA',        module = quant_2 },
            #serviceTask { name='Team',       module = quant_2 },
            #serviceTask { name='Validation', module = quant_2, prompt=[#approve{}] },
            #serviceTask { name='Succ',       module = quant_2 },
            #endEvent    { name='Final'}
        ],

        beginEvent = 'Init',
        endEvent = 'Final',
        events = [
             #messageEvent{name="Valid"}
        ]
    }.

action({request,'Validation'}, Proc) ->
    case bpe:doc(#approve{},Proc) of
         #approve{} -> {reply,'Succ',Proc};
         _ -> {reply,Proc} end;

action(_,Proc) -> {reply,Proc}.


