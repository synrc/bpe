require BPE

defmodule BPE.Test do
  use ExUnit.Case

  test "compare" do
    assert Test.Compare.test() == :ok
  end

  test "service" do
    assert Test.Service.test() == :ok
  end

  test "process group" do
    count = 5
    name  = :kvs.seq([], [])
    Enum.each(1..count, fn _ ->
      BPE.start(Test.Compare.def(), [], {BPE.monitor(id: name), BPE.procRec()})
    end)
    group = :kvs.feed(~c"/bpe/mon/" ++ name)
    assert length(group) == count
  end
end
