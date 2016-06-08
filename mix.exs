defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [app: :bpe,
     version: "1.0.0",
     description: "Erlang Business Process Engine",
     package: package,
     deps: deps
    ]
  end

  def application do
    [mod: {:bpe_app, []},
     applications: [:mnesia,
                    :n2o,
                    :kvs]]
  end

  defp package do
    [files: ~w(include src LICENSE mix.exs README.md rebar.config),
     licenses: ["MIT"],
     links: %{"GitHub" => "https://github.com/spawnproc/bpe"}]
  end

  defp deps do[
    {:kvs,                          github: "synrc/kvs", tag: "2.9"},
    {:active,                       github: "synrc/active"},
    {:n2o,                          github: "rixmann/n2o", branch: "fix/configure_as_mix_application"},
    {:pgsql,                        github: "epgsql/epgsql", tag: "2.0.0", app: false},
  ]
  end
end
