defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [app: :bpe,
     version: "1.0",
     description: "Erlang Business Process Engine",
     package: package]
  end

  defp package do
    [files: ~w(include src LICENSE mix.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/spawnproc/bpe"}]
   end
end
