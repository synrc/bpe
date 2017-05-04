defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [app: :bpe,
     version: "2.4.0",
     description: "BPE Process Engine",
     package: package,
     deps: deps]
  end

  def application do
    [mod: {:bpe, []}]
  end

  defp package do
    [files: ["include", "priv", "src", "LICENSE", "README.md", "rebar.config"],
     licenses: ["MIT"],
     maintainers: ["Andy Martemyanov", "Namdak Tonpa"],
     name: :bpe,
     links: %{"GitHub" => "https://github.com/synrc/bpe"}]
  end

  defp deps do
     [{:ex_doc, ">= 0.0.0", only: :dev}]
  end
end
