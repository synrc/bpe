
ExUnit.start()
:kvs.join()
require BPE

defmodule BPE.Test do
  use ExUnit.Case, async: true
  test "compare" do
    assert Test.Compare.test == :ok
  end
  test "service" do
    assert Test.Service.test == :ok
  end
  test "process group" do
    count = 5
    name = :kvs.seq([],[])
    :lists.map(fn _ ->:bpe.start Test.Compare.def, [], BPE.monitor(id: name) end, :lists.seq(1,count))
    group = :kvs.feed '/bpe/mon/' ++ name
    assert count == length group
  end
end
