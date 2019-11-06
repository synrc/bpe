defmodule Test.Compare do
  require BPE
  import BPE

  def def(), do: xml("/priv/compare.bpmn")
  def auth(_), do: true
  def action(_message,state), do: {:reply,state}

  @doc_type :direction

  def test() do
    test0()
    test1()
    test2()
    :ok
  end

  def test0() do
    {:ok, pid} = :bpe.start def(), []
    {:complete, 'some'} = :bpe.next pid
    {:complete, 'default'} = :bpe.next pid
    {:complete, 'any'} = :bpe.next pid
    {:complete, 'epilog'} = :bpe.next pid
    {:complete, 'finish'} = :bpe.next pid
    :Final = :bpe.next pid
  end

  def test1() do
    {:ok, pid} = :bpe.start def(), []
    {:complete, 'some'} = :bpe.amend pid, {@doc_type, :right}
    {:complete, 'right'} = :bpe.next pid
    {:complete, 'any'} = :bpe.next pid
    {:complete, 'epilog'} = :bpe.next pid
    {:complete, 'finish'} = :bpe.next pid
    :Final = :bpe.next pid
  end

  def test2() do
    {:ok, pid} = :bpe.start def(), []
    {:complete, 'some'} = :bpe.amend pid, {@doc_type, :left}
    {:complete, 'left'} = :bpe.next pid
    {:complete, 'any'} = :bpe.next pid
    {:complete, 'epilog'} = :bpe.next pid
    {:complete, 'finish'} = :bpe.next pid
    :Final = :bpe.next pid
  end

end
