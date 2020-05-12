defmodule BPE do
  require Record

  defmacro xml(source) do
    mod = __CALLER__.module
    IO.inspect Mix.Project.app_path()
    file = Mix.Project.app_path() <> source
    proc = Macro.escape(:bpe_xml.load(to_charlist(file), mod))
    quote do
      unquote(proc)
    end
  end

  Enum.each(Record.extract_all(from_lib: "bpe/include/bpe.hrl"), fn {name, definition} ->
    Record.defrecord(name, definition)
  end)

end
