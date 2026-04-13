import Config

config :kvs,
  dba: :kvs_mnesia,
  dba_st: :kvs_stream,
  dba_seq: :kvs_mnesia,
  schema: [:kvs, :kvs_stream, BPE.Metainfo]
