-module(bpe_trace).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("bpe/include/bpe.hrl").
-include_lib("nitro/include/nitro.hrl").

doc() -> "This is the actor trace table row representation in FORMS CSS. Used to draw trace of the processes"
         " in <a href=\"actors.htm\">BPE process table</a> but displayed as class=form.".
id() -> #hist{task={task,'Init'}}.
new(Name,Hist) ->
    {_,Task} = element(#hist.task,Hist),
    Docs = element(#hist.docs,Hist),
    #panel { id=forms:atom([tr,nitro:to_list(Name)]),
             class=td,
             body=[
        #panel{class=column6,   body = io_lib:format("~s",[nitro:to_list(Task)]) },
        #panel{class=column20,  body = string:join(lists:map(fun(X)-> nitro:to_list([element(1,X)]) end,Docs),", ")}
       ]}.
