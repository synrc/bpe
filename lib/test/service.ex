defmodule Test.Service do
  require BPE
  import BPE

  def def(), do: xml("/priv/service.bpmn")
  def auth(_), do: true
  def action(_message, state), do: result(state: state)

  @doc_type :direction

  def dir_right(proc), do: [{@doc_type, :right}] === BPE.doc({@doc_type}, proc)
  def dir_left(proc),  do: [{@doc_type, :left}]  === BPE.doc({@doc_type}, proc)

  def test() do
    test0()
    test1()
    test2()
    :ok
  end

  def test0() do
    {:ok, pid} = BPE.start(def(), [])
    {:complete, ~c"some"} = BPE.next(pid)
    :Final = BPE.next(pid)
  end

  def test1() do
    {:ok, pid} = BPE.start(def(), [])
    {:complete, ~c"some"}   = BPE.amend(pid, {@doc_type, :right})
    {:complete, ~c"right"}  = BPE.next(pid)
    {:complete, ~c"any"}    = BPE.next(pid)
    {:complete, ~c"epilog"} = BPE.next(pid)
    {:complete, ~c"finish"} = BPE.next(pid)
    :Final = BPE.next(pid)
  end

  def test2() do
    {:ok, pid} = BPE.start(def(), [])
    {:complete, ~c"some"}   = BPE.amend(pid, {@doc_type, :left})
    {:complete, ~c"left"}   = BPE.next(pid)
    {:complete, ~c"any"}    = BPE.next(pid)
    {:complete, ~c"epilog"} = BPE.next(pid)
    {:complete, ~c"finish"} = BPE.next(pid)
    :Final = BPE.next(pid)
  end
end
