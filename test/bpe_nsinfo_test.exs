defmodule BPE.NSinfoTest do
  use ExUnit.Case

  require BPE
  import BPE

  test "loads exgate1 bpmn" do
    _doc = xml("/priv/exgate1.bpmn")
    assert true
  end
end
