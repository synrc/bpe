-module(quant_1).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("doc.hrl").
-compile(export_all).

def() ->

    #process { name = 'SpawnConference',

        flows = [
            #sequenceFlow{source='Init',      target='SelectConf'},
            #sequenceFlow{source='SelectConf', target='AssignPersons'},
            #sequenceFlow{source='AssignPersons', target='Reduce'},
            #sequenceFlow{source='Reduce',   target=['SelectConf','Final']}
        ],

        tasks = [
            #userTask    { name='Init',      module = quant_1 },
            #userTask    { name='SelectConf', module = quant_1 },
            #serviceTask { name='AssignPersons',  module = quant_1 },
            #serviceTask { name='Reduce',  module = quant_1 },
            #endEvent    { name='Final'}
        ],

        beginEvent = 'Init',
        endEvent = 'Final',
        events = [
             #messageEvent{name="Reduce"}
        ]
    }.

action({request,_}, Proc) ->
    {reply,Proc}.

