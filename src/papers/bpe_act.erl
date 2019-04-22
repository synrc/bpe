-module(bpe_act).
-copyright('Maxim Sokhatsky').
-include_lib("nitro/include/nitro.hrl").
-include_lib("forms/include/meta.hrl").
-include("bpe.hrl").
-compile(export_all).
-record(pi, {code='Spawnproc'}).

event(init) ->
   nitro:clear(tableHead),
   Bin = nitro:qc(p),
   Id = try binary_to_integer(Bin) catch _:_ -> 0 end,
   case kvs:get(process,Id) of
        {error,not_found} ->
           nitro:update(n, "ERR"),
           nitro:update(desc, "No process found."),
           nitro:update(num, "ERR");
        _ ->
           nitro:insert_top(tableHead, header()),
           nitro:update(n, Bin),
           nitro:update(num, Bin),
   History = bpe:hist(Id),
 [ nitro:insert_bottom(tableHead,
   bpe_trace:new(forms:atom([trace,nitro:to_list(I#hist.id)]),I))
   || I <- History ]
   end;

event(E) ->
   io:format("Event:process:~p~n.",[E]),
   ok.

header() ->
  #panel{id=header,class=th,body=
    [#panel{class=column6,body="State"},
     #panel{class=column6,body="Documents"}]}.

doc() -> "Dialog for creation of BPE processes.".
id() -> #pi{}.
new(Name,{pi,_Code}) ->
  #document { name = forms:atom([pi,Name]), sections = [
      #sec { name=[<<"New process: "/utf8>>] } ],
    buttons  = [ #but { id=forms:atom([pi,decline]),
                        title= <<"Discard"/utf8>>,
                        class=cancel,
                        postback={'Discard',[]} },
                 #but { id=forms:atom([pi,proceed]),
                        title = <<"Create"/utf8>>,
                        class = [button,sgreen],
                        sources = [process_type],
                        postback = {'Spawn',[]}}],
    fields = [ #field { name=process_type,
                        id=process_type,
                        type=select,
                        title= "Type",
                        tooltips = [],
                        options = [ #opt{name=quant_1,title = "Client Acquire [QUANTERALL]"},
                                    #opt{name=quant_2,title = "Client Tracking [QUANTERALL]"},
                                    #opt{name=bpe_account,checked=true,title = "Client Account [SYNRC BANK]"},
                                    #opt{name=tour,title = "Tournaments [ESM.ONE]"}
                       ]}]}.
