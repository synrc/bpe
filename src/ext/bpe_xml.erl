-module(bpe_xml).
-include_lib("bpe/include/bpe.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).

load() ->
  {ok,Bin} = file:load_file("priv/diagram_1.bpmn"),
  {#xmlElement{name=N,attributes=L},X} = xmerl_scan:string(Bin),
  io:format("Name ~p Attr ~p Tail ~p~n",[N,L,X]),
  ok.
