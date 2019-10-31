
:kvs.join
ExUnit.start()

defmodule BPE.Test do
  use ExUnit.Case, async: true
  test "compare" do
    assert Test.Compare.test == :ok
  end
  test "service" do
    assert Test.Service.test == :ok
  end
end
