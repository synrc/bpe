-module(tour).
-include("bpe.hrl").
-include("doc.hrl").
-compile(export_all).

def() ->

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
%             #boundaryEvent{ name = '*', timeout={0,{0,30,0}} },
             #messageEvent{name='PaymentReceived'}
        ]
    }.

action({request,'Init'}, Proc)       ->
    {reply,Proc#process{docs=[#max_tour{count=3},#tour_list{users=[]}]}};
action({request,'JoinTeams'}, Proc)  ->
    #max_tour{count=N,joined=M} = bpe:doc(#max_tour{}, Proc),
    J = #join_application{}     = bpe:doc(#join_application{}, Proc),
    #tour_list{users=Users}     = bpe:doc(#tour_list{}, Proc),
    Modify = [#max_tour{count=N,joined=M+1},#tour_list{users=[J|Users]}],
    Proc2 = Proc#process{docs=Modify},
    case M + 1 == N of
         false -> {reply,'JoinTeams',Proc2};
         true  -> {reply,'EmitGroups',Proc2} end;
action({request,'EmitGroups'}, Proc) -> {reply,Proc};
action({request,'RunGroups'}, Proc)  -> {reply,Proc};
action({request,'BuildBracket'}, Proc)  -> {reply,Proc};
action({request,'Eliminate'}, Proc)  -> {reply,Proc};
action({request,'Final'}, Proc)      -> {reply,Proc}.

test() ->
    case bpe:start(tour:def(),[]) of
      {error,_} -> skip;
      {ok,Id} ->
    bpe:complete(Id),
    bpe:amend(Id,#join_application{name=vlad,data=1}),
    bpe:amend(Id,#join_application{name=doxtop,data=2}),
    bpe:amend(Id,#join_application{name=maxim,data=3}),
    bpe:complete(Id),
    bpe:process(Id)
       end.
