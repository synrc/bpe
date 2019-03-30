-module(bpe_process).
-copyright('Maxim Sokhatsky').
-include_lib("nitro/include/nitro.hrl").
-include_lib("forms/include/meta.hrl").
-include("bpe.hrl").
-compile(export_all).
-record(pi, {code='Spawnproc'}).

main() -> [].

event(init) ->
   nitro:clear(tableHead),
   nitro:insert_top(tableHead, header()),
   Bin = nitro:qc(p),
   Id = binary_to_integer(Bin),
   io:format("Id: ~p~n",[Id]),
   case bpe:hist(Id) of
        []  -> skip;
        History -> nitro:update(n, Bin),
                  nitro:update(num, Bin),
               [ nitro:insert_bottom(tableHead, bpe_trace:new(forms:atom([trace,nitro:to_list(I#hist.id)]),I))
               || I <- History ],
                ok
   end,
   ok;

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
                        options = [ #opt{name=spawnproc,checked=true,title = "Spawnproc [PB]"},
                                    #opt{name=tour,checked=true,title = "Tournaments [ESM.ONE]"}
                       ]}]}.
