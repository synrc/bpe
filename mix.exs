defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [
      app: :bpe,
      version: "4.6.0",
      description: "BPE Process Engine",
      package: package(),
      deps: deps()
    ]
  end

  def application do
    [mod: {:bpe_app, []}, applications: [:syn, :n2o, :kvx]]
  end

  def package do
    [
      files: ["include", "priv", "src", "LICENSE", "README.md", "rebar.config"],
      licenses: ["MIT"],
      maintainers: ["Namdak Tonpa"],
      name: :bpe,
      links: %{"GitHub" => "https://github.com/synrc/bpe"}
    ]
  end

  def deps do
    [
      {:syn, github: "ostinelli/syn", tag: "1.5.0"},
      {:rocksdb, github: "voxoz/rocks"},
      {:kvx, github: "synrc/kvx"}
    ]
  end
end
