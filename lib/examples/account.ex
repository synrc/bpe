defmodule BPE.Account do
  @moduledoc """
  Bank Account Process.

  This module serves as example of long-term banking process formalization in SYNRC/BPE.
  """
  require BPE
  import BPE

  def auth(_), do: true

  def def do
    p = process(
      name:   "IBAN Account",
      module: __MODULE__,
      flows: [
        sequenceFlow(id: "->Init",              source: "Created",   target: "Init"),
        sequenceFlow(id: "->Upload",            source: "Init",      target: "Upload"),
        sequenceFlow(id: "->Payment",           source: "Upload",    target: "Payment"),
        sequenceFlow(id: "Payment->Signatory",  source: "Payment",   target: "Signatory"),
        sequenceFlow(id: "Payment->Process",    source: "Payment",   target: "Process"),
        sequenceFlow(id: "Process-loop",        source: "Process",   target: "Process"),
        sequenceFlow(id: "Process->Final",      source: "Process",   target: "Final"),
        sequenceFlow(id: "Signatory->Process",  source: "Signatory", target: "Process"),
        sequenceFlow(id: "Signatory->Final",    source: "Signatory", target: "Final")
      ],
      tasks: [
        beginEvent(id: "Created"),
        userTask(id: "Init"),
        userTask(id: "Upload"),
        userTask(id: "Signatory"),
        serviceTask(id: "Payment"),
        serviceTask(id: "Process"),
        endEvent(id: "Final")
      ],
      beginEvent: "Created",
      endEvent:   "Final",
      events: [
        messageEvent(id: "PaymentReceived"),
        boundaryEvent(id: :*, timeout: timeout(spec: {0, {10, 0, 10}}))
      ]
    )
    process(p, tasks: BPE.XML.fill_in_out(process(p, :tasks), process(p, :flows)))
  end

  def action({:request, "Created", _}, proc),   do: result(type: :reply, state: proc)
  def action({:request, "Init", _}, proc),      do: result(type: :reply, state: proc)

  def action({:request, "Payment", _}, proc) do
    case BPE.doc({:payment_notification}, proc) do
      [] -> result(type: :reply, reply: "Process",   state: process(proc, docs: [{:tx}]))
      _  -> result(type: :reply, reply: "Signatory", state: proc)
    end
  end

  def action({:request, "Signatory", _}, proc) do
    result(type: :reply, reply: "Process", state: proc)
  end

  def action({:request, "Process", _}, proc) do
    case BPE.doc({:close_account}, proc) do
      [{:close_account}] -> result(type: :reply, reply: "Final",   state: proc)
      _                  -> result(type: :reply, reply: "Process", state: proc)
    end
  end

  def action({:request, "Upload", _}, proc),    do: result(type: :reply, state: proc)
  def action({:request, "Final", _}, proc),     do: result(type: :stop,  state: proc)
end
