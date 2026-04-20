defmodule BPE.Ticket do
  require BPE
  import BPE

  def auth(_), do: true

  def def do
    p = process(
      name: "Support Ticket",
      module: __MODULE__,
      flows: [
        sequenceFlow(id: "->Assign", source: "Create", target: "Assign"),
        sequenceFlow(id: "Assign->Work", source: "Assign", target: "Work"),
        sequenceFlow(id: "Work->Resolve", source: "Work", target: "Resolve"),
        sequenceFlow(id: "Resolve->Closed", source: "Resolve", target: "Closed"),
        # Escalation path (triggered by SLA boundary event + manual loop-back)
        sequenceFlow(id: "Escalate->Work", source: "Escalate", target: "Work")
      ],
      tasks: [
        beginEvent(id: "Create"),
        userTask(id: "Assign"),
        userTask(id: "Work"),
        serviceTask(id: "Escalate"),
        userTask(id: "Resolve"),
        endEvent(id: "Closed")
      ],
      beginEvent: "Create",
      endEvent: "Closed",
      events: [
        messageEvent(id: "TicketUpdate"),
        # SLA timer attached to Work task (interrupting boundary → Escalate)
        boundaryEvent(id: :*, timeout: timeout(spec: {0, {24, 0, 0}}))
      ]
    )
    process(p, tasks: BPE.XML.fill_in_out(process(p, :tasks), process(p, :flows)))
  end

  # All action handlers (control sequenceFlow selection via :reply when multiple outgoing flows exist)
  def action({:request, "Create", _}, proc), do: result(type: :reply, state: proc)
  def action({:request, "Assign", _}, proc), do: result(type: :reply, state: proc)
  def action({:request, "Work", _}, proc), do: result(type: :reply, state: proc)
  def action({:request, "Escalate", _}, proc) do
    # Escalation service (e.g. notify manager, add escalation flag)
    result(type: :reply, reply: "Work", state: process(proc, docs: [{:escalated, true}]))
  end
  def action({:request, "Resolve", _}, proc), do: result(type: :reply, state: proc)
  def action({:request, "Closed", _}, proc), do: result(type: :stop, state: proc)
end