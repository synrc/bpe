defmodule BPE.CompareTest do
  use ExUnit.Case

  require BPE
  import BPE

  def def(), do: xml("/priv/compare.bpmn")
  def auth(_), do: true
  def action(_message, state), do: result(state: state)

  @doc_type :direction

  test "test0" do
    {:ok, pid} = BPE.start(def(), [])
    assert {:complete, ~c"some"}    = BPE.next(pid)
    assert {:complete, ~c"default"} = BPE.next(pid)
    assert {:complete, ~c"any"}     = BPE.next(pid)
    assert {:complete, ~c"epilog"}  = BPE.next(pid)
    assert {:complete, ~c"finish"}  = BPE.next(pid)
    assert :Final = BPE.next(pid)
  end

  test "test1" do
    {:ok, pid} = BPE.start(def(), [])
    assert {:complete, ~c"some"}   = BPE.amend(pid, {@doc_type, :right})
    assert {:complete, ~c"right"}  = BPE.next(pid)
    assert {:complete, ~c"any"}    = BPE.next(pid)
    assert {:complete, ~c"epilog"} = BPE.next(pid)
    assert {:complete, ~c"finish"} = BPE.next(pid)
    assert :Final = BPE.next(pid)
  end

  test "test2" do
    {:ok, pid} = BPE.start(def(), [])
    assert {:complete, ~c"some"}   = BPE.amend(pid, {@doc_type, :left})
    assert {:complete, ~c"left"}   = BPE.next(pid)
    assert {:complete, ~c"any"}    = BPE.next(pid)
    assert {:complete, ~c"epilog"} = BPE.next(pid)
    assert {:complete, ~c"finish"} = BPE.next(pid)
    assert :Final = BPE.next(pid)
  end
end
