defmodule BPE.ProcessTest do
  use ExUnit.Case

  setup_all do
    {:ok, _} = Application.ensure_all_started(:syn)
    :ok = :syn.add_node_to_scopes([:devices])
    {:ok, _} = Application.ensure_all_started(:bpe)
    :ok
  end

  test "ticketing system – full lifecycle with SLA escalation" do
    IO.puts("\n=== Starting Ticket Process ===")
    {:ok, id} = BPE.start(BPE.Ticket.def(), [])
    IO.inspect(id, label: "BPE.start (Ticket)")

    assert {:complete, "Assign"} = BPE.next(id)
    assert {:complete, "Work"} = BPE.next(id)

    BPE.amend(id, {:sla_violation, true})
    assert {:complete, "Closed"} = BPE.next(id)
    assert :Final = BPE.next(id)

  end

  test "e-commerce order – abandoned cart recovery + payment flow (Intershop style)" do
    IO.puts("\n=== Starting Order Process ===")
    {:ok, id} = BPE.start(BPE.Order.def(), [])
    IO.inspect(id, label: "BPE.start (Order)")

    assert {:complete, "CartActive"} = BPE.next(id)
    assert {:complete, "Checkout"} = BPE.next(id)

    BPE.amend(id, {:payment_received, "tx_12345"})
    assert {:complete, "Fulfilment"} = BPE.next(id)
    assert {:complete, "PostPurchase"} = BPE.next(id)
    assert {:complete, "Delivered"} = BPE.next(id)
    assert :Final = BPE.next(id)

  end
end
