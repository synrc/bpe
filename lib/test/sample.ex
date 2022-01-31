defmodule Test.Sample do
  require BPE
  import BPE

  def def(), do: xml("/priv/sample.bpmn")
  def auth(_), do: true
  def action(_message,state), do: BPE.result(state: state)

  def test() do
    {:ok, pid} = :bpe.start def(), []
    {:complete, 'either'} = :bpe.next(pid)
    {:complete, 'left'} = :bpe.next(pid, 'x2')
    {:complete, 'right'} = :bpe.next(pid, 'x3')
    {:complete, 'join'} = :bpe.next(pid, 'x5')
    {:error, _, _} = :bpe.next(pid, 'x6')
    {:complete, 'join'} = :bpe.next(pid, 'x4')
    {:complete, 'epilog'} = :bpe.next(pid)
    {:complete, 'finish'} = :bpe.next(pid)
    :Final = :bpe.next(pid)
    :ok
  end

end

