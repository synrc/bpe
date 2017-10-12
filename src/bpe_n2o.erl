-module(bpe_n2o).
-compile({parse_transform, bert_javascript}).
-compile(export_all).

info(M,R,S) -> {unknown,M,R,S}.
