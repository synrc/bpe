-module(bpe_forms).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

event({client,{form,Module}}) ->
    nitro:insert_bottom(stand, #h3{body=nitro:to_binary(Module)}),
    nitro:insert_bottom(stand, #h5{body=Module:doc(),style="margin-bottom: 10px;"}),
    nitro:insert_bottom(stand, (forms:new(Module:new(Module,Module:id()), Module:id()))#panel{class=form});

event(init) ->
    nitro:clear(stand),
    [ self() ! {client,{form,F}} || F <- application:get_env(forms, registry, []) ],
    ?LOG_INFO("HELO.~n",[]);

event({Event,Name}) ->
    nitro:wire(lists:concat(["console.log(\"",io_lib:format("~p",[{Event,Name}]),"\");"])),
    ?LOG_INFO("Event:~p.~n", [{Event,Name}]);

event(Event) ->
    ?LOG_INFO("Unknown:~p.~n", [Event]).
