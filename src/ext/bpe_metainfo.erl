-module(bpe_metainfo).
-include("bpe.hrl").
-include_lib("kvs/include/metainfo.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = bpe, tables = [
          #table{name = process, fields=record_info(fields, process)},
          #table{name = procMonitor, fields=record_info(fields, procMonitor)},
          #table{name = procRec, fields=record_info(fields, procRec)},
          #table{name = hist, fields=record_info(fields, hist)},
          #table{name = sched, fields=record_info(fields, sched)}
         ]}.

