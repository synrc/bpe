-module(tour).
-include("bpe.hrl").
-include("tour.hrl").
-compile(export_all).

def() -> tour_process:definition().

action({request,'Init'}, Proc)       ->
    {reply,Proc#process{docs=[#max_tour{count=3},#tour_list{users=[]}]}};
action({request,'JoinTeams'}, Proc)  ->
    #max_tour{count=N,joined=M} = bpe:doc(#max_tour{}, Proc),
    J = #join_application{}     = bpe:doc(#join_application{}, Proc),
    #tour_list{users=Users}     = bpe:doc(#tour_list{}, Proc),
    Modify = [#max_tour{count=N,joined=M+1},#tour_list{users=[J|Users]}],
    Proc2 = bpe:add_recs(Proc,Modify),
    case M + 1 == N of
         false -> {reply,'JoinTeams',Proc2};
         true  -> {reply,'EmitGroups',Proc2} end;
action({request,'EmitGroups'}, Proc) -> {reply,Proc};
action({request,'RunGroups'}, Proc)  -> {reply,Proc};
action({request,'Final'}, Proc)      -> {reply,Proc}.

test() ->
    {ok,Id} = bpe:start(tour:def(),[]),
    bpe:complete(Id),
    bpe:amend(Id,#join_application{name=vlad,data=1}),
    bpe:amend(Id,#join_application{name=doxtop,data=2}),
    bpe:amend(Id,#join_application{name=maxim,data=3}),
    bpe:complete(Id),
    bpe:process(Id).
