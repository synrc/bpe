defmodule BPE.Order do
  @moduledoc """
  Order Process.

  This module serves as example of long-term banking process formalization in SYNRC/BPE.
  """
  require BPE
  import BPE

  def auth(_), do: true

  def def do
    p = process(
      name: "E-Commerce Order",
      module: __MODULE__,
      flows: [
        sequenceFlow(id: "->CartActive", source: "CartCreated", target: "CartActive"),
        sequenceFlow(id: "CartActive->Checkout", source: "CartActive", target: "Checkout"),
        sequenceFlow(id: "AbandonedRecovery->CartActive", source: "AbandonedRecovery", target: "CartActive"),
        sequenceFlow(id: "Checkout->Payment", source: "Checkout", target: "Payment"),
        sequenceFlow(id: "Payment->Fulfilment", source: "Payment", target: "Fulfilment"),
        sequenceFlow(id: "Fulfilment->PostPurchase", source: "Fulfilment", target: "PostPurchase"),
        sequenceFlow(id: "PostPurchase->Delivered", source: "PostPurchase", target: "Delivered")
      ],
      tasks: [
        beginEvent(id: "CartCreated"),
        userTask(id: "CartActive"),
        serviceTask(id: "AbandonedRecovery"),
        userTask(id: "Checkout"),
        serviceTask(id: "Payment"),
        serviceTask(id: "Fulfilment"),
        serviceTask(id: "PostPurchase"),
        endEvent(id: "Delivered")
      ],
      beginEvent: "CartCreated",
      endEvent: "Delivered",
      events: [
        messageEvent(id: "PaymentReceived"),
        messageEvent(id: "ShipmentUpdate"),
        # Abandoned-cart timer attached to CartActive (interrupting boundary → recovery)
        boundaryEvent(id: :*, timeout: timeout(spec: {0, {1, 0, 0}}))
      ]
    )
    process(p, tasks: BPE.XML.fill_in_out(process(p, :tasks), process(p, :flows)))
  end

  # All action handlers (realistic Intershop-style decisions via documents + :reply for flow control)
  def action({:request, "CartCreated", _}, proc), do: result(type: :reply, state: proc)

  def action({:request, "CartActive", _}, proc), do: result(type: :reply, state: proc)

  def action({:request, "AbandonedRecovery", _}, proc) do
    # Realistic recovery: send email/SMS, add recovery doc for analytics
    result(
      type: :reply,
      reply: "CartActive",
      state: process(proc, docs: [{:abandoned_recovery_sent, true}])
    )
  end

  def action({:request, "Checkout", _}, proc), do: result(type: :reply, state: proc)

  def action({:request, "Payment", _}, proc) do
    # Payment service task – waits for external gateway via messageEvent + document
    case BPE.doc({:payment_received}, proc) do
      [] ->
        # Still waiting for confirmation (e.g. 3D-Secure, webhook pending)
        result(type: :reply, reply: "Payment", state: proc)

      _ ->
        # Payment confirmed → proceed (add transaction for fulfilment)
        result(
          type: :reply,
          reply: {:complete, "Fulfilment"},
          state: process(proc, docs: [{:tx, :paid}])
        )
    end
  end

  def action({:request, "Fulfilment", _}, proc) do
    # Fulfilment service (inventory, shipping label, etc.)
    # Can be extended with ShipmentUpdate message later
    result(type: :reply, state: proc)
  end

  def action({:request, "PostPurchase", _}, proc) do
    # Post-purchase events (order confirmation email, review request, loyalty points)
    result(
      type: :reply,
      state: process(proc, docs: [{:post_purchase_sent, true}])
    )
  end

  def action({:request, "Delivered", _}, proc), do: result(type: :stop, state: proc)
end
