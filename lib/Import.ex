defmodule BPE.Import do
  defmacro xml(source) do
    mod = __CALLER__.module
    quote do
      file = File.cwd!() <> unquote(source)
      :bpe_xml.load(to_charlist(file), unquote(mod))
    end
  end
end

