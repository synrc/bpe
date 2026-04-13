import Config

config :kvs,
  dba: :kvs_rocks,
  dba_st: :kvs_st,
  dba_seq: :kvs_rocks,
  schema: [:kvs, :kvs_stream, BPE.Metainfo]

config :bpe,
  logger_level: :debug,
  logger: [{:handler, :synrc, :logger_std_h,
            %{level: :debug,
              id: :synrc,
              module: :logger_std_h,
              config: %{type: :file, file: ~c"bpe.log"},
              formatter: {:logger_formatter,
                          %{template: [:time, ~c" ", :pid, ~c" ", :msg, ~c"\n"],
                            single_line: true,}}}}]
