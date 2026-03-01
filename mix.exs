defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [
      app: :bpe,
      version: "9.9.6",
      description: "BPE Business Process Engine",
      package: package(),
      deps: deps()
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

  def deps do
    [
      {:ex_doc, "~> 0.40", only: :dev},
      {:syn, "~> 3.4"},
      {:rocksdb, "~> 2.5"},
      {:kvs, "~> 13.3.1"}
    ]
  end
end
