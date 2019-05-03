-module(bpe_table).
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("bpe/include/bpe.hrl").
-include_lib("nitro/include/nitro.hrl").

header() ->
  #thead{id=thead,body=
    [#th{body="No"},
     #th{body="Name"},
     #th{body="Credit"}
     ]}.

body() ->
  #tbody{id=tbody,body=[#tr{cells=[#td{body="1"},#td{body="maxim"},#td{body="100"}]},
                        #tr{cells=[#td{body="2"},#td{body="doxtop"},#td{body="100"}]},
                        #tr{cells=[#td{body="3"},#td{body="vlad"},#td{body="100"}]},
                        #tr{cells=[#td{body="4"},#td{body="eugene"},#td{body="1000"}]}
                       ]}.

event(init) ->
    nitro:clear(content),
    nitro:insert_top(content, #table{header=header(),body=body()}),
    ok;

event(Event) ->
    ?LOG_INFO("Unknown:~p.~n", [Event]).
