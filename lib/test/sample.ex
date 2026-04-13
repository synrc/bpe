defmodule Test.Sample do
  require BPE
  import BPE

  def def(), do: xml("/priv/sample.bpmn")
  def auth(_), do: true
  def action(_message, state), do: result(state: state)

  def test() do
    {:ok, pid} = BPE.start(def(), [])
    {:complete, ~c"either"} = BPE.next(pid)
    {:complete, ~c"left"}   = BPE.next(pid, ~c"x2")
    {:complete, ~c"right"}  = BPE.next(pid, ~c"x3")
    {:complete, ~c"join"}   = BPE.next(pid, ~c"x5")
    {:error, _, _}          = BPE.next(pid, ~c"x6")
    {:complete, ~c"join"}   = BPE.next(pid, ~c"x4")
    {:complete, ~c"epilog"} = BPE.next(pid)
    {:complete, ~c"finish"} = BPE.next(pid)
    :Final = BPE.next(pid)
    :ok
  end
end
