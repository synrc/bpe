-module(bpe_account).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("doc.hrl").
-export([def/0]).
-compile(export_all).

def() ->
    #process { name = 'IBAN Account',
        flows = [
            #sequenceFlow{source='Created',   target='Init'},
            #sequenceFlow{source='Init',      target='Upload'},
            #sequenceFlow{source='Upload',    target='Payment'},
            #sequenceFlow{source='Payment',   target=['Signatory','Process']},
            #sequenceFlow{source='Process',   target=['Process','Final']},
            #sequenceFlow{source='Signatory', target=['Process','Final']} ],
        tasks = [
            #beginEvent  { name='Created',   module = bpe_account },
            #userTask    { name='Init',      module = bpe_account },
            #userTask    { name='Upload',    module = bpe_account },
            #userTask    { name='Signatory', module = bpe_account },
            #serviceTask { name='Payment',   module = bpe_account },
            #serviceTask { name='Process',   module = bpe_account },
            #endEvent    { name='Final',     module = bpe_account } ],
        beginEvent = 'Created',
        endEvent = 'Final',
        events = [ #messageEvent{name='PaymentReceived'},
                   #boundaryEvent{name='*', timeout={0, {10, 0, 10}}} ] }.

action({request,'Created',_}, Proc) ->
    {reply,Proc};

action({request,'Init',_}, Proc) ->
    {reply,Proc};

action({request,'Payment',_}, Proc) ->
    Payment = bpe:doc({payment_notification},Proc),
    case Payment of
         [] -> {reply,'Process',Proc#process{docs=[#tx{}]}};
          _ -> {reply,'Signatory',Proc} end;

action({request,'Signatory',_}, Proc) ->
    {reply,'Process',Proc};

action({request,'Process',_}, Proc) ->
    case bpe:doc(#close_account{},Proc) of
         #close_account{} -> {reply,'Final',Proc};
                        _ -> {reply,Proc} end;

action({request,'Upload',_}, Proc) ->
    {reply,Proc};

action({request,'Final',_}, Proc) ->
    {reply,Proc}.
