use Mix.Config

config :kvs,
  dba: :kvs_rocks,
  dba_st: :kvs_st,
  schema: [:kvs, :kvs_stream, :bpe_metainfo]

config :bpe,
  logger_level: :debug,
  logger: [{:handler, :synrc, :logger_std_h,
            %{formatter: {:logger_formatter, %{single_line: true, template: [:time,' ',:pid,' ',:msg,'\n']}},
              level: :debug,
              id: :synrc,
              module: :logger_std_h,
              config: %{type: :file, file: 'bpe.log'}}} ]
