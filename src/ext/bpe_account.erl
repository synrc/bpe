-module(bpe_account).
-author('Maxim Sokhatsky').
-include("bpe.hrl").
-include("doc.hrl").
-export([def/0,auth/1,action/2]).

% use bpe:complete with this BPMN 1.0 process

auth(_) -> true.

def() ->
  P =  #process { name = "IBAN Account",
        module = ?MODULE,
        flows = [
            #sequenceFlow { id="->Init", source="Created", target="Init"},
            #sequenceFlow { id="->Upload", source="Init", target="Upload"},
            #sequenceFlow { id="->Payment", source="Upload", target="Payment"},
            #sequenceFlow { id="Payment->Signatory", source="Payment", target="Signatory"},
            #sequenceFlow { id="Payment->Process", source="Payment", target="Process"},
            #sequenceFlow { id="Process-loop", source="Process", target="Process"},
            #sequenceFlow { id="Process->Final", source="Process", target="Final"},
            #sequenceFlow { id="Signatory->Process", source="Signatory", target="Process"},
            #sequenceFlow { id="Signatory->Final", source="Signatory", target="Final"} ],
        tasks = [
            #beginEvent { id="Created" },
            #userTask { id="Init" },
            #userTask { id="Upload" },
            #userTask { id="Signatory" },
            #serviceTask { id="Payment" },
            #serviceTask { id="Process" },
            #endEvent { id="Final" } ],
        beginEvent = "Created",
        endEvent = "Final",
        events = [ #messageEvent{id="PaymentReceived"},
                   #boundaryEvent{id='*', timeout=#timeout{spec={0, {10, 0, 10}}}} ] },

   P#process{tasks = bpe_xml:fillInOut(P#process.tasks,P#process.flows)}.

action({request,"Created",_}, Proc) ->
    #result{type=reply,state=Proc};

action({request,"Init",_}, Proc) ->
    #result{type=reply,state=Proc};

action({request,"Payment",_X}, Proc) ->
    Payment = bpe:doc({payment_notification},Proc),
    case Payment of
         [] -> #result{type=reply,reply="Process",state=Proc#process{docs=[#tx{}]}};
          _ -> #result{type=reply,reply="Signatory",state=Proc} end;

action({request,"Signatory",_}, Proc) ->
    #result{type=reply,reply="Process",state=Proc};

action({request,"Process",_X}, Proc) ->
    case bpe:doc(#close_account{},Proc) of
         [#close_account{}] -> #result{type=reply,reply="Final",state=Proc};
                          _ -> #result{type=reply,reply="Process",state=Proc} end;

action({request,"Upload",_}, Proc) ->
    #result{type=reply,state=Proc};

action({request,"Final",_}, Proc) ->
    #result{type=stop,state=Proc}.
