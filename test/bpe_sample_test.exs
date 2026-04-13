defmodule BPE.SampleTest do
  use ExUnit.Case

  require BPE
  import BPE

  def def(), do: xml("/priv/sample.bpmn")
  def auth(_), do: true
  def action(_message, state), do: result(state: state)

  test "sample process trace runs through gateways" do
    {:ok, pid} = BPE.start(def(), [])
    assert {:complete, ~c"either"} = BPE.next(pid)
    assert {:complete, ~c"left"}   = BPE.next(pid, ~c"x2")
    assert {:complete, ~c"right"}  = BPE.next(pid, ~c"x3")
    assert {:complete, ~c"join"}   = BPE.next(pid, ~c"x5")
    assert {:error, _, _}          = BPE.next(pid, ~c"x6")
    assert {:complete, ~c"join"}   = BPE.next(pid, ~c"x4")
    assert {:complete, ~c"epilog"} = BPE.next(pid)
    assert {:complete, ~c"finish"} = BPE.next(pid)
    assert :Final = BPE.next(pid)
  end
end
