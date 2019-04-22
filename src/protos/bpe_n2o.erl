-module(bpe_n2o).
-include("bpe.hrl").
-include("doc.hrl").
-record('Token', {data= [] :: binary()}).
-record(io, {code= [] :: term(),data = [] :: [] | #'Token'{} | #process{} | #io{} | term() }).

-compile({parse_transform, bert_javascript}).
-compile(export_all).

info(#'Amen'{id=Proc,docs=Docs},R,S) -> {reply,{bert,#io{data=bpe:amend(Proc,Docs)}},R,S};
info(#'Hist'{id=Proc},R,S)           -> {reply,{bert,#io{data=bpe:hist(Proc)}},      R,S};
info(#'Proc'{id=Proc},R,S)           -> {reply,{bert,#io{data=bpe:process(Proc)}},   R,S};
info(#'Load'{id=Proc},R,S)           -> {reply,{bert,#io{data=bpe:load(Proc)}},      R,S};
info(#'Comp'{id=Proc},R,S)           -> {reply,{bert,#io{data=bpe:complete(Proc)}},  R,S};
info(#'Make'{proc=M,docs=Docs},R,S)  -> {reply,{bert,#io{data=bpe:start((nitro:to_atom(M)):def(),Docs)}},R,S};
info(M,R,S)                          -> {unknown,M,R,S}.
