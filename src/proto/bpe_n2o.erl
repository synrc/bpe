-module(bpe_n2o).
-include("bpe.hrl").
-include("doc.hrl").
-record('Token', {data= [] :: binary()}).
-record(io, {code= [] :: term(),data = [] :: [] | #'Token'{} | #process{} | #io{} }).
-compile({parse_transform, bert_javascript}).
-compile(export_all).

info(#amend{id=Proc,docs=Docs}=M,R,S) -> {reply,{bert,#io{data=bpe:amend(Proc,Docs)}},R,S};
info(#histo{id=Proc}=M,R,S) -> {reply,{bert,#io{data=bpe:hist(Proc)}},R,S};
info(#proc{id=Proc}=M,R,S) -> {reply,{bert,#io{data=bpe:process(Proc)}},R,S};
info(#load{id=Proc}=M,R,S) -> {reply,{bert,#io{data=bpe:load(Proc)}},R,S};
info(#complete{id=Proc}=M,R,S) -> {reply,{bert,#io{data=bpe:complete(Proc)}},R,S};
info(#create{proc=Module,docs=Docs}=M,R,S) ->
    {reply,{bert,#io{data=bpe:start((nitro:to_atom(Module)):def(),Docs)}},R,S};

info(M,R,S) -> {unknown,M,R,S}.
