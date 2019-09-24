-module(bpe_xml).
-include_lib("bpe/include/bpe.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-compile(export_all).

attr(E) -> [ {N,V} || #xmlAttribute{name=N,value=V} <- E].

find(E,[]) -> [ {X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E];
find(E, I) -> [ {X,find(Sub,[]),attr(A)} || #xmlElement{name=X,attributes=A,content=Sub} <- E, X == I].

load() ->
  {ok,Bin} = file:read_file("priv/diagram_1.bpmn"),
  {#xmlElement{name=N,content=C}=X,_} = xmerl_scan:string(binary_to_list(Bin)),
  E = {N,find(C,'bpmn:process'),attr(C)},
  io:format("Compact Form ~p~n",[E]),
  ok.
