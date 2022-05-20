-module(bpe_metainfo).
-include("bpe.hrl").
-include_lib("kvs/include/metainfo.hrl").
-export([metainfo/0]).

metainfo() ->
    #schema{name = bpe, tables = [
          #table{name = process, fields=record_info(fields, process), instance = #process{}},
          #table{name = monitor, fields=record_info(fields, monitor), instance = #monitor{}},
          #table{name = procRec, fields=record_info(fields, procRec), instance = #procRec{}},
          #table{name = hist, fields=record_info(fields, hist), instance = #hist{}},
          #table{name = sched, fields=record_info(fields, sched), instance = #sched{}},
          #table{name = subscription, fields=record_info(fields, subscription), instance = #subscription{}, keys=record_info(fields, subscription)}
         ]}.

