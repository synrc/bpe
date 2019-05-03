-module(bpe_login).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("n2o/include/n2o.hrl").
-include_lib("nitro/include/nitro.hrl").

event(init) ->
    nitro:clear(stand),
      Module = bpe_otp,
      nitro:insert_bottom(stand,
                forms:new(Module:new(Module,Module:id()), Module:id()));

event({'Next',_}) ->
    nitro:redirect("actors.htm");

event({'Close',_}) ->
    nitro:redirect("index.html");

event(Event) ->
    ?LOG_INFO("Unknown:~p.~n", [Event]).
