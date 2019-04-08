defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [app: :bpe,
     version: "4.4.0",
     description: "BPE Process Engine",
     package: package,
     deps: deps]
  end

  def application do
        [mod: {:bpe_app, []},
         applications: [:mnesia,
                        :n2o,
                        :kvs]]
  end

  defp package do
    [files: ["include", "priv", "src", "LICENSE", "README.md", "rebar.config"],
     licenses: ["MIT"],
     maintainers: ["Namdak Tonpa"],
     name: :bpe,
     links: %{"GitHub" => "https://github.com/synrc/bpe"}]
  end

  defp deps do[
        {:kvs,                          github: "synrc/kvs"},
        {:active,                       github: "synrc/active"},
        {:n2o,                          github: "synrc/n2o"},
      ]
  end

end
