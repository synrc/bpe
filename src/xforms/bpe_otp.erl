-module(bpe_otp).
-copyright('Maxim Sokhatsky').
-include_lib("forms/include/meta.hrl").
-compile(export_all).
-record(phone, {code="+380490000000",number=[]}).

doc() -> "One-time password PIN control used in banks,".
id() -> #phone{}.
new(Name,_Phone) ->
    #document { name = forms:atom([otp,Name]),
    sections = [ #sec { name=[<<"Input the credentials: "/utf8>> ] } ],
    buttons  = [ #but { id=decline,
                        name=decline,
                        title= <<"Cancel"/utf8>>,
                        class=cancel,
                        postback={'Close',[]} },
                 #but { id=proceed,
                        name=proceed,
                        title = <<"Proceed"/utf8>>,
                        class = [button,sgreen],
                        sources = [user,otp],
                        postback = {'Next',forms:atom([otp,otp,Name])}}],
    fields = [ #field { id=user,
                        name=user,
                        type=string,
                        title= <<"Login:"/utf8>>,
                        labelClass=label,
                        pos=2,
                        fieldClass=column3},
               #field { id=otp,
                        name=otp,
                        type=otp,
                        title= <<"Pass:"/utf8>>,
                        labelClass=label,
                        pos=3,
                        fieldClass=column3}]}.
