defmodule BPE.Mixfile do
  use Mix.Project

  def project do
    [
      app: :bpe,
      version: "11.4.14",
      description:
        "ERP/1 BPMN ДСТУ ISO/IEC 19510:2015 Business Process Model Notation",
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
    deps = [
      {:ex_doc, ">= 0.0.0", only: :dev},
      {:syn, "~> 3.4"},
      {:kvs, "~> 13.4.13"}
    ]

    if System.get_env("BPE_BACKEND") == "rocksdb" do
      deps ++ [{:rocksdb, "~> 2.5", optional: true}]
    else
      deps
    end
  end
end
