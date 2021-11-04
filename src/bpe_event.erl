-module(bpe_event).

-author('Maxim Sokhatsky').

-export([event_action/5, handle_event/3]).

-include("bpe.hrl").

event_action(Module, _Name, Event, Target, Proc) ->
    case Module:action({event,
                        Event#messageEvent.name,
                        Event#messageEvent.payload},
                       Proc)
        of
        {reply, State} -> {reply, {complete, Target}, State};
        {reply, Task, State} -> {reply, {complete, Task}, State}
    end.

handle_event(#beginEvent{}, Target, Proc) ->
    {reply, {complete, Target}, Proc};
handle_event(#endEvent{}, Target, Proc) ->
    {stop, {normal, Target}, Proc};
handle_event(#messageBeginEvent{}, Target, Proc) ->
    {stop, {normal, Target}, Proc};
handle_event(#messageEvent{name = Name} = Event, Target,
             #process{module = Module} = Proc) ->
    event_action(Module, Name, Event, Target, Proc);
handle_event(_, Target, Proc) ->
    {reply, {unknown_event, Target}, Proc}.
