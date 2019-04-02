-module(bpe_n2o).
-include("bpe.hrl").
-include("doc.hrl").
-compile({parse_transform, bert_javascript}).
-compile(export_all).

info(#create{proc= <<"tour">>,docs=D}=M,R,S) ->
    n2o:info(?MODULE, "create:~w",[M]),
    {reply,{bert,{io,bpe:start(tour:def(),D), <<>>}},R,S};

info(#create{proc=Proc,docs=Docs}=M,R,S) ->
    n2o:info(?MODULE, "create:~w",[M]),
    {reply,{bert,{io,bpe:start(Proc,Docs), <<>>}},R,S};

info(#amend{id=Proc,docs=Docs}=M,R,S) ->
    n2o:info(?MODULE, "amend:~w",[M]),
    {reply,{bert,{io,bpe:amend(Proc,Docs), <<>>}},R,S};

info(#histo{id=Proc}=M,R,S) ->
    n2o:info(?MODULE, "hist:~w",[M]),
    {reply,{bert,{io,bpe:hist(Proc), <<>>}},R,S};

info(#proc{id=Proc}=M,R,S) ->
    n2o:info(?MODULE, "proc:~w",[M]),
    {reply,{bert,{io,bpe:process(Proc), <<>>}},R,S};

info(#complete{id=Proc}=M,R,S) ->
    n2o:info(?MODULE, "complete:~w",[M]),
    {reply,{bert,{io,bpe:complete(Proc),      <<>>}},R,S};

info(M,R,S) -> {unknown,M,R,S}.
