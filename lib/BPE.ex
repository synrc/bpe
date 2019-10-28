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

  defmacro __using__(opts \\ []) do
    imports =
      opts
      |> Macro.expand(__CALLER__)
      |> Keyword.get(:with, [:bpe])

    Enum.map(imports, fn mod ->
      if Code.ensure_compiled?(mod) do
        upcased = Module.concat([String.upcase(to_string(mod))])

        quote do
          import unquote(upcased)
          alias unquote(mod), as: unquote(upcased)
        end
      else
        IO.warn("🚨 Unknown module #{mod} was requested to be used by :bpe. Skipping.")
      end
    end)
  end
end
