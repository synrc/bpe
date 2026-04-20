import Config

backend = System.get_env("BPE_BACKEND") || "mnesia"

if backend == "rocksdb" do
  import_config "config.rocksdb.exs"
else
  import_config "config.mnesia.exs"
end

config :logger, level: :debug

config :syn, scopes: [:devices]

config :bpe,
  logger_level: :debug,
  logger: [
    {:handler, :synrc, :logger_std_h,
     %{
       level: :debug,
       id: :synrc,
       module: :logger_std_h,
       config: %{type: :file, file: ~c"bpe.log"},
       formatter:
         {:logger_formatter,
          %{
            template: [:time, ~c" ", :pid, ~c" ", :msg, ~c"\n"],
            single_line: true
          }}
     }}
  ]
