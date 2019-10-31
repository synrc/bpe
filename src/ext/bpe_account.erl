-module(bpe_account).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("doc.hrl").
-export([def/0,auth/1]).
-compile(export_all).

% use bpe:complete with this BPMN 1.0 process

auth(_) -> true.

def() ->
  P =  #process { name = "IBAN Account",
        flows = [
            #sequenceFlow{id="->Init", source="Created",   target="Init"},
            #sequenceFlow{id="->Upload", source="Init",      target="Upload"},
            #sequenceFlow{id="->Payment", source="Upload",    target="Payment"},
            #sequenceFlow{id="Payment->Signatory", source="Payment",   target="Signatory"},
            #sequenceFlow{id="Payment->Process", source="Payment",   target="Process"},
            #sequenceFlow{id="Process-loop", source="Process",   target="Process"},
            #sequenceFlow{id="Process->Final", source="Process",   target="Final"},
            #sequenceFlow{id="Signatory->Process", source="Signatory", target="Process"},
            #sequenceFlow{id="Signatory->Final", source="Signatory", target="Final"} ],
        tasks = [
            #beginEvent  { id="Created",   module = bpe_account },
            #userTask    { id="Init",      module = bpe_account },
            #userTask    { id="Upload",    module = bpe_account },
            #userTask    { id="Signatory", module = bpe_account },
            #serviceTask { id="Payment",   module = bpe_account },
            #serviceTask { id="Process",   module = bpe_account },
            #endEvent    { id="Final",     module = bpe_account } ],
        beginEvent = "Created",
        endEvent = "Final",
        events = [ #messageEvent{id="PaymentReceived"},
                   #boundaryEvent{id='*', timeout=#timeout{spec={0, {10, 0, 10}}}} ] },

   P#process{tasks = bpe_xml:fillInOut(P#process.tasks,P#process.flows)}.

action({request,"Created",_}, Proc) ->
    {reply,Proc};

action({request,"Init",_}, Proc) ->
    {reply,Proc};

action({request,"Payment",X}, Proc) ->
    Payment = bpe:doc({payment_notification},Proc),
    case Payment of
         [] -> {reply,"Process",Proc#process{docs=[#tx{}]}};
          _ -> {reply,"Signatory",Proc} end;

action({request,"Signatory",_}, Proc) ->
    {reply,"Process",Proc};

action({request,"Process",X}, Proc) ->
    case bpe:doc(#close_account{},Proc) of
         [#close_account{}] -> {reply,"Final",Proc};
                          _ -> {reply,"Process",Proc} end;

action({request,"Upload",_}, Proc) ->
    {reply,Proc};

action({request,"Final",_}, Proc) ->
    {reply,Proc}.
