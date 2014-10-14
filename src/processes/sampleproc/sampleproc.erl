-module(sampleproc).
-author('Maxim Sokhatsky').
-include_lib("bpe/include/bpe.hrl").
-include_lib("kvs/include/user.hrl").
-compile(export_all).

definition() -> sampleproc_process:definition().

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

