-module(bpe_metainfo).
-include_lib("kvs/include/metainfo.hrl").
-include_lib("kvs/include/kvs.hrl").
-include_lib("kvs/include/config.hrl").
-include("bpe.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = bpe, tables = [
          #table{name = process, fields=record_info(fields, process)},
          #table{name = hist, fields=record_info(fields, hist)}
%          #table{name = sequenceFlow, fields=record_info(fields, sequenceFlow)},
%          #table{name = userTask, fields=record_info(fields, userTask)},
%          #table{name = serviceTask, fields=record_info(fields, serviceTask)},
%          #table{name = messageEvent, fields=record_info(fields, messageEvent)},
%          #table{name = beginEvent, fields=record_info(fields, beginEvent)},
%          #table{name = endEvent, fields=record_info(fields, endEvent)}
         ]}.

mnesia(copy)->
    #schema{tables =Tables}=metainfo(),
    [ekka_mnesia:copy_table(Tab,disc_copies) || Tab <- Tables].
