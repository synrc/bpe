-module(bpe_login).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

main() -> [].
body() -> [].

event(init) ->
    nitro:clear(stand),
      Module = bpe_otp,
      nitro:insert_bottom(stand,
                forms:new(Module:new(Module,Module:id()), Module:id()));

event({Event,Name}) ->
    nitro:wire(lists:concat(["console.log(\"",io_lib:format("~p",[{Event,Name}]),"\");"])),
    n2o:info(?MODULE,"Event:~p.~n", [{Event,nitro:q(nitro:to_atom(Name))}]),
    nitro:redirect("index.html");

event(Event) ->
    n2o:info(?MODULE,"Unknown:~p.~n", [Event]).
