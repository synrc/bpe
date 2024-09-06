import Config

config :kvs,
  dba: :kvs_rocks,
  dba_st: :kvs_st,
  dba_seq: :kvs_rocks,
  schema: [:kvs, :kvs_stream, :bpe_metainfo]

config :bpe,
  logger_level: :debug,
  logger: [{:handler, :synrc, :logger_std_h,
            %{level: :debug,
              id: :synrc,
              module: :logger_std_h,
              config: %{type: :file, file: 'bpe.log'},
              formatter: {:logger_formatter,
                          %{template: [:time,' ',:pid,' ',:msg,'\n'],
                            single_line: true,}}}}]
