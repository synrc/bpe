-module(bpe_process).
-copyright('Maxim Sokhatsky').
-include_lib("forms/include/meta.hrl").
-compile(export_all).
-record(pi, {code='Spawnproc'}).

main() -> [].

event(init) ->
   nitro:update(n, nitro:qc(p)),
   nitro:update(num, nitro:qc(p)),
   ok;

event(E) ->
   io:format("Event:process:~p~n.",[E]),
   ok.

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
