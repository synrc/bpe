-module (routes).
-author('Maxim Sokhatsky').
-include_lib("n2o/include/wf.hrl").
-export(?ROUTING_API).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, Ctx) -> 
    Path = wf:path(Ctx#cx.req),
	wf:info("ROute: ~p",[Path]),
    Module = route_prefix(Path),
    {ok, State, Ctx#cx{path=Path,module=Module}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>)              -> bpe;
route(<<"index">>)       -> bpe;
route(<<"favicon.ico">>) -> static_file;
route(_) -> bpe.
