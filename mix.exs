defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [
      app: :bpe,
      version: "11.4.14",
      description: "ERP/1 BPMN ДСТУ ISO/IEC 19510:2015 Business Process Model Notation",
      package: package(),
      deps: deps()
    ]
  end

  def application do
    [mod: {BPE.OTP, []}, extra_applications: [:xmerl, :syn, :kvs]]
  end

  def package do
    [
      files: ~w(include man lib config priv mix.exs LICENSE README.md),
      licenses: ["ISC"],
      maintainers: ["Namdak Tonpa"],
      name: :bpe,
      links: %{"GitHub" => "https://github.com/synrc/bpmn"}
    ]
  end

  def deps do
    [
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:syn, "~> 3.4"},
      {:rocksdb, "~> 2.5", optional: true},
      {:kvs, "~> 13.4.13"}
    ]
  end
end
