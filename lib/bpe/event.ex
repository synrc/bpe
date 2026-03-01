defmodule BPE.Event do
  require BPE
  import BPE

  def event_action(module, _name, event, _target, proc) do
    BPE.construct_result(
      apply(module, :action, [
        {:event, messageEvent(event, :name), messageEvent(event, :payload)},
        proc
      ])
    )
  end

  def handle_event(beginEvent(), target, proc),        do: {:reply, {:complete, target}, proc}
  def handle_event(endEvent(), target, proc),          do: {:stop, {:normal, target}, proc}
  def handle_event(messageBeginEvent(), target, proc), do: {:stop, {:normal, target}, proc}

  def handle_event(messageEvent(name: name) = event, target, process(module: module) = proc) do
    event_action(module, name, event, target, proc)
  end

  def handle_event(_, target, proc), do: {:reply, {:unknown_event, target}, proc}
end
