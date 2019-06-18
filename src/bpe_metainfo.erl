-module(bpe_metainfo).
-include("bpe.hrl").
-include_lib("kvx/include/metainfo.hrl").
-compile(export_all).

metainfo() ->
    #schema{name = bpe, tables = [
          #table{name = process, fields=record_info(fields, process)},
          #table{name = hist, fields=record_info(fields, hist)}
         ]}.

mnesia(copy)->
    #schema{tables =Tables}=metainfo(),
      [ekka_mnesia:copy_table(Tab,disc_copies) || Tab <- Tables].
