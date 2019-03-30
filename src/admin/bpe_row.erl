-module(bpe_row).
-copyright('Maxim Sokhatsky').
-compile(export_all).
-include_lib("bpe/include/bpe.hrl").
-include_lib("nitro/include/nitro.hrl").

doc() -> "This is the actor table row representation in FORMS CSS. Used to draw active processes"
         " in <a href=\"actors.htm\">BPE process table</a> but displayed as class=form.".
id() -> #process{}.
new(Name,Proc) ->
    Pid = nitro:to_list(Proc#process.id),
    #panel { id=forms:atom([tr,Name]),
             class=td,
             body=[
        #panel{class=column6,   body = #link{href="act.htm?p="++Pid, body=Pid } },
        #panel{class=column6,   body = nitro:to_list(Proc#process.name) },
        #panel{class=column6,   body = nitro:to_list(element(#task.module,bpe:task(Proc#process.task,Proc)))},
        #panel{class=column20,  body = nitro:to_list(Proc#process.task)},
        #panel{class=column20,  body = string:join(lists:map(fun(X)-> nitro:to_list([element(1,X)]) end,
                                       element(#task.prompt,bpe:task(Proc#process.task,Proc))),", ")},
        #panel{class=column10,  body = case Proc#process.task of 'Final' -> [];
                                       _ -> [ #link{postback={complete,Proc#process.id}, class=[button,sgreen],
                                         body= "Go", source=[], validate=[]} ] end }
       ]}.
