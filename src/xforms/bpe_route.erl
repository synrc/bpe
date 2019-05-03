-module(bpe_route).
-include_lib("n2o/include/n2o.hrl").
-export([init/2, finish/2]).

finish(State, Ctx) -> {ok, State, Ctx}.
init(State, #cx{req=Req}=Cx) ->
    #{path:=Path}=Req,
    Fix  = route_prefix(Path),
    {ok, State, Cx#cx{path=Path,module=Fix}}.

route_prefix(<<"/ws/",P/binary>>) -> route(P);
route_prefix(<<"/",P/binary>>) -> route(P);
route_prefix(P) -> route(P).

route(<<>>)                      -> bpe_index;
route(<<"actors",_/binary>>)     -> bpe_index;
route(<<"login",_/binary>>)      -> bpe_login;
route(<<"forms",_/binary>>)      -> bpe_forms;
route(<<"table",_/binary>>)      -> bpe_table;
route(<<"act",_/binary>>)        -> bpe_act;
route(<<"app/actors",_/binary>>) -> bpe_index;
route(<<"app/login",_/binary>>)  -> bpe_login;
route(<<"app/forms",_/binary>>)  -> bpe_forms;
route(<<"app/table",_/binary>>)  -> bpe_table;
route(<<"app/act",_/binary>>)    -> bpe_act;
route(_)                         -> bpe_login.
