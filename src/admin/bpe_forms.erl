-module(bpe_forms).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

main() -> [].
body() -> [].

event({client,{form,Module}}) ->
      X = nitro:insert_bottom(stand, #h3{body=nitro:to_binary(Module)}),
      Y = nitro:insert_bottom(stand, #h5{body=Module:doc(),style="margin-bottom: 10px;"}),
      Z = nitro:insert_bottom(stand, (forms:new(Module:new(Module,Module:id()), Module:id()))#panel{class=form}),
      io:format("FORM: ~p~n",[{Module,X,Y,Z}]),
      [];

event(init) ->
    nitro:clear(stand),
    [ self() ! {client,{form,F}} || F <- application:get_env(forms, registry, []) ],
    n2o:info(?MODULE,"HELO.~n",[]);

event({Event,Name}) ->
    io:format("IO: ~p~n",[{Event,Name}]),
    nitro:wire(lists:concat(["console.log(\"",io_lib:format("~p",[{Event,Name}]),"\");"])),
    n2o:info(?MODULE,"Event:~p.~n", [{Event,Name}]);

event(Event) ->
    n2o:info(?MODULE,"Unknown:~p.~n", [Event]).
