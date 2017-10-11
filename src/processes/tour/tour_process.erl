-module(tour_process).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-compile(export_all).

definition() ->

    #process { name = 'Sample Tournament',

        flows = [
            #sequenceFlow{source='Init',         target='JoinTeams'},
            #sequenceFlow{source='JoinTeams',    target='JoinTeams'},
            #sequenceFlow{source='JoinTeams',    target='EmitGroups'},
            #sequenceFlow{source='EmitGroups',   target='RunGroups'},
            #sequenceFlow{source='RunGroups',    target='BuildBracket'},
            #sequenceFlow{source='BuildBracket', target='Eliminate'},
            #sequenceFlow{source='Eliminate',    target='Final'}
        ],

        tasks = [
            #serviceTask { name='Init',           module = tour },
            #userTask    { name='JoinTeams',      module = tour },
            #userTask    { name='EmitGroups',     module = tour },
            #serviceTask { name='RunGroups',      module = tour },
            #serviceTask { name='BuildBracket',   module = tour },
            #serviceTask { name='Eliminate',      module = tour },
            #endEvent    { name='Final',          module = tour }
        ],

        beginEvent = 'Init',
        endEvent = 'Final',
        events = [
             #boundaryEvent{ name = '*', timeout={0,{0,30,0}} },
             #messageEvent{name="PaymentReceived"}
        ]
    }.
