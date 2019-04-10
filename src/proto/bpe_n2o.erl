-module(bpe_n2o).
-include("bpe.hrl").
-include("doc.hrl").
-record('Token', {data= [] :: binary()}).
-record(io, {code= [] :: term(),data = [] :: [] | #'Token'{} | #process{} | #io{} }).
-compile({parse_transform, bert_javascript}).
-compile(export_all).

info(#create{proc=Module,docs=Docs}=M,R,S) ->
    n2o:info(?MODULE, "create:~tp",[M]),
    Proc = nitro:to_atom(Module),
    {reply,{bert,#io{data=bpe:start(Proc:def(),Docs)}},R,S};

info(#amend{id=Proc,docs=Docs}=M,R,S) ->
    n2o:info(?MODULE, "amend:~tp",[M]),
    {reply,{bert,#io{data=bpe:amend(Proc,Docs)}},R,S};

info(#histo{id=Proc}=M,R,S) ->
    n2o:info(?MODULE, "hist:~tp",[M]),
    {reply,{bert,#io{data=bpe:hist(Proc)}},R,S};

info(#proc{id=Proc}=M,R,S) ->
    n2o:info(?MODULE, "proc:~tp",[M]),
    {reply,{bert,#io{data=bpe:process(Proc)}},R,S};

info(#load{id=Proc}=M,R,S) ->
    n2o:info(?MODULE, "proc:~tp",[M]),
    {reply,{bert,#io{data=bpe:load(Proc)}},R,S};

info(#complete{id=Proc}=M,R,S) ->
    n2o:info(?MODULE, "complete:~tp",[M]),
    {reply,{bert,#io{data=bpe:complete(Proc)}},R,S};

info(M,R,S) -> {unknown,M,R,S}.
