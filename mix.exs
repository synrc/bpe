defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [
      app: :bpe,
      version: "4.10.22",
      description: "BPE Business Process Engine",
      package: package(),
      deps: deps()
    ]
  end

  def application do
    [mod: {:bpe_otp, []}, applications: [:rocksdb, :syn, :kvs]]
  end

  def package do
    [
      files: ~w(include lib priv src mix.exs rebar.config),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :bpe,
      links: %{"GitHub" => "https://github.com/synrc/bpe"}
    ]
  end

  def deps do
    [
      {:ex_doc, "~> 0.11", only: :dev},
      {:rocksdb, "~> 1.3.2"},
      {:syn, "~> 1.6.3"},
      {:kvs, "~> 6.10.1"}
    ]
  end
end
