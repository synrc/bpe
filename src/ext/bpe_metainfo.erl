-module(bpe_metainfo).
-include("bpe.hrl").
-include_lib("kvs/include/metainfo.hrl").
-export([metainfo/0]).

metainfo() ->
    #schema{name = bpe, tables = [
          #table{name = process, fields=record_info(fields, process)},
          #table{name = monitor, fields=record_info(fields, monitor)},
          #table{name = procRec, fields=record_info(fields, procRec)},
          #table{name = hist, fields=record_info(fields, hist)},
          #table{name = sched, fields=record_info(fields, sched)}
         ]}.

