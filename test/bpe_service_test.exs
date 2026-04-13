defmodule BPE.ServiceTest do
  use ExUnit.Case

  require BPE
  import BPE

  def def(), do: xml("/priv/service.bpmn")
  def auth(_), do: true
  def action(_message, state), do: result(state: state)

  @doc_type :direction

  def dir_right(proc), do: [{@doc_type, :right}] === BPE.doc({@doc_type}, proc)
  def dir_left(proc),  do: [{@doc_type, :left}]  === BPE.doc({@doc_type}, proc)

  test "service default branch" do
    {:ok, pid} = BPE.start(def(), [])
    assert {:complete, ~c"some"} = BPE.next(pid)
    assert :Final = BPE.next(pid)
  end

  test "service right branch" do
    {:ok, pid} = BPE.start(def(), [])
    assert {:complete, ~c"some"}   = BPE.amend(pid, {@doc_type, :right})
    assert {:complete, ~c"right"}  = BPE.next(pid)
    assert {:complete, ~c"any"}    = BPE.next(pid)
    assert {:complete, ~c"epilog"} = BPE.next(pid)
    assert {:complete, ~c"finish"} = BPE.next(pid)
    assert :Final = BPE.next(pid)
  end

  test "service left branch" do
    {:ok, pid} = BPE.start(def(), [])
    assert {:complete, ~c"some"}   = BPE.amend(pid, {@doc_type, :left})
    assert {:complete, ~c"left"}   = BPE.next(pid)
    assert {:complete, ~c"any"}    = BPE.next(pid)
    assert {:complete, ~c"epilog"} = BPE.next(pid)
    assert {:complete, ~c"finish"} = BPE.next(pid)
    assert :Final = BPE.next(pid)
  end
end
