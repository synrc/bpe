defmodule BPE.Task do
  require BPE
  import BPE

  def find_flow(:noflow), do: []
  def find_flow([h | _]) when is_list(h), do: h
  def find_flow([h | _] = list) when is_integer(h), do: list

  def find_flow([], list), do: find_flow(list)

  def find_flow(stage, list) do
    if Enum.member?(list, stage), do: stage, else: find_flow(list)
  end

  def move_doclink({_source, _target}, _proc), do: []

  def targets(name, proc) do
    List.flatten(
      for sequenceFlow(source: source, target: target) <- process(proc, :flows),
          source == name,
          do: target
    )
  end

  def denied_flow(curr, proc),    do: {:reply, {:denied_flow, curr}, proc}
  def already_finished(proc),     do: {:stop, {:normal, []}, proc}

  def task_action(module, source, target, proc) do
    {m, f, a} =
      :proplists.get_value(:flow_callback, process(proc, :etc), {BPE.Task, :move_doclink, [{source, target}, proc]})

    case apply(module, :action, [{:request, source, target}, proc]) do
      result(continue: c) = x when c != [] ->
        result(x, opt: {:continue, c})
      result(type: :reply, reply: []) = x ->
        apply(m, f, a)
        result(x, reply: {:complete, target})
      result(type: :reply) = x ->
        apply(m, f, a)
        x
      x -> x
    end
  end

  def handle_task(beginEvent(), curr, target, proc) do
    task_action(process(proc, :module), curr, target, proc)
  end

  def handle_task(userTask(), curr, target, process(module: mod) = proc) do
    task_action(mod, curr, target, proc)
  end

  def handle_task(receiveTask(), curr, target, process(module: mod) = proc) do
    task_action(mod, curr, target, proc)
  end

  def handle_task(sendTask(), curr, target, process(module: mod) = proc) do
    task_action(mod, curr, target, proc)
  end

  def handle_task(serviceTask(), curr, target, process(module: mod) = proc) do
    task_action(mod, curr, target, proc)
  end

  def handle_task(gateway(type: :parallel), src, dst, process(module: mod) = proc) do
    task_action(mod, src, dst, proc)
  end

  def handle_task(endEvent(), curr, target, process(module: mod) = proc) do
    task_action(mod, curr, target, proc)
    {:stop, {:normal, target}, proc}
  end

  def handle_task(_, _, target, proc), do: {:reply, {:unknown_task, target}, proc}
end
