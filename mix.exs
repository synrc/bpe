defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [
      app: :bpe,
      version: "9.9.7",
      description: "BPE Business Process Engine",
      package: package(),
      deps: deps(),
      docs: docs()
    ]
  end

  def application do
    [mod: {BPE.OTP, []}, applications: [:xmerl, :syn, :kvs]]
  end

  def package do
    [
      files: ~w(include lib priv mix.exs),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :bpe,
      links: %{"GitHub" => "https://github.com/synrc/bpe"}
    ]
  end

  def docs do
    [
      main: "BPE",
      extras: ["README.md"],
      skip_undefined_reference_warnings_on: [:all],

      # Much stricter filter + only Elixir modules
      filter_modules: fn mod, _ ->
        mod_str = Atom.to_string(mod)
        String.starts_with?(mod_str, "Elixir.BPE") and
        not String.starts_with?(mod_str, "Elixir.Test.")
      end,

      # Extra safety
      ignore_apps: [:ex_doc, :eex]
    ]
  end

  def deps do
    [
      {:ex_doc, "~> 0.40", only: :dev},
      {:syn, "~> 3.4"},
      {:rocksdb, "~> 2.5"},
      {:kvs, "~> 13.3.1"}
    ]
  end
end
