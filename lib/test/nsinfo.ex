defmodule Test.NSinfo do
  require BPE
  import BPE

  def test() do
    _doc = xml("/priv/exgate1.bpmn")
    :ok
  end
end
