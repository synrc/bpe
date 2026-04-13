defmodule BPE.XML do
  require Record
  require BPE

  @model :"http://www.omg.org/spec/BPMN/20100524/MODEL"

  for {name, fields} <- Record.extract_all(from_lib: "xmerl/include/xmerl.hrl") do
    Record.defrecord(name, fields)
  end

  def ns(xmlElement(name: n, nsinfo: [], namespace: xmlNamespace(default: @model)) = e, s) do
    n1 = :"bpmn:#{n}"
    {xmlElement(e, name: n1, nsinfo: {"bpmn", n}), s}
  end

  def ns(xmlElement(name: n, nsinfo: [], namespace: xmlNamespace(default: [], nodes: nds)) = e, s) do
    case :lists.keyfind(@model, 2, nds) do
      {p, _} ->
        n1 = :"#{p}:#{n}"
        {xmlElement(e, name: n1, nsinfo: {p, n}), s}
      false -> {e, s}
    end
  end

  def ns(e, s), do: {e, s}

  def attr(e) do
    for xmlAttribute(name: n, value: v) <- e, do: {n, v}
  end

  def find(e, []) do
    for xmlElement(name: x, attributes: a, content: sub) <- e do
      case x do
        :"bpmn:conditionExpression" ->
          vs = for xmlText(value: v) <- sub, do: v
          {x, vs, attr(a)}
        :"bpmn:flowNodeRef" ->
          [v | _] = for xmlText(value: v) <- sub, do: v
          {x, [], {:value, v}}
        _ ->
          {x, find(sub, []), attr(a)}
      end
    end
  end

  def find(e, i) do
    for xmlElement(name: x, attributes: a, content: sub) <- e, x == i do
      {x, find(sub, []), attr(a)}
    end
  end

  def load(file), do: load(file, __MODULE__)

  def load(file, module) do
    case :xmerl_scan.file(file, [{:hook_fun, &ns/2}]) do
      {:error, r} -> {:error, r}
      {xmlElement(name: _n, content: c), _} ->
        {_n2, [{:"bpmn:process", elements, attrs}], _} =
          {:definitions, find(c, :"bpmn:process"), attr(c)}
        id   = :proplists.get_value(:id, attrs)
        name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
        proc = reduce(elements, BPE.process(id: id, name: name, module: module))
        tasks1 = fill_in_out(BPE.process(proc, :tasks), BPE.process(proc, :flows))
        tasks2 = fix_roles(tasks1, BPE.process(proc, :roles))
        BPE.process(proc, id: [], tasks: tasks2, xml: :filename.basename(file, ~c".bpmn"), events: BPE.process(proc, :events))
    end
  end

  def reduce([], acc), do: acc

  def reduce([{:"bpmn:task", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id   = :proplists.get_value(:id, attrs)
    name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.task(id: id, name: name) | tasks]))
  end

  def reduce([{:"bpmn:startEvent", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id   = :proplists.get_value(:id, attrs)
    name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.beginEvent(id: id, name: name) | tasks], beginEvent: id))
  end

  def reduce([{:"bpmn:endEvent", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id   = :proplists.get_value(:id, attrs)
    name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.endEvent(id: id, name: name) | tasks], endEvent: id))
  end

  def reduce([{:"bpmn:sequenceFlow", body, attrs} | t], BPE.process(flows: flows) = proc) do
    id     = :proplists.get_value(:id, attrs)
    name   = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    source = :proplists.get_value(:sourceRef, attrs)
    target = :proplists.get_value(:targetRef, attrs)
    f      = BPE.sequenceFlow(id: id, name: name, source: source, target: target)
    flow   = reduce(body, f)
    reduce(t, BPE.process(proc, flows: [flow | flows]))
  end

  def reduce([{:"bpmn:conditionExpression", body, _attrs} | t], BPE.sequenceFlow() = flow) do
    reduce(t, parse_expression(flow, parse(hd(body))))
  end

  def reduce([{:"bpmn:parallelGateway", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id   = :proplists.get_value(:id, attrs)
    name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.gateway(id: id, name: name, type: :parallel) | tasks]))
  end

  def reduce([{:"bpmn:exclusiveGateway", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id      = :proplists.get_value(:id, attrs)
    default = :proplists.get_value(:default, attrs, [])
    name    = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.gateway(id: id, name: name, type: :exclusive, def: default) | tasks]))
  end

  def reduce([{:"bpmn:inclusiveGateway", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id   = :proplists.get_value(:id, attrs)
    name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.gateway(id: id, name: name, type: :inclusive) | tasks]))
  end

  def reduce([{:"bpmn:complexGateway", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id   = :proplists.get_value(:id, attrs)
    name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.gateway(id: id, name: name, type: :complex) | tasks]))
  end

  def reduce([{:"bpmn:gateway", _body, attrs} | t], BPE.process(tasks: tasks) = proc) do
    id   = :proplists.get_value(:id, attrs)
    name = :unicode.characters_to_binary(:proplists.get_value(:name, attrs, []))
    reduce(t, BPE.process(proc, tasks: [BPE.gateway(id: id, name: name) | tasks]))
  end

  def reduce([{:"bpmn:laneSet", lanes, _attrs} | t], proc) do
    roles =
      for {_, tasks_xml, att} <- lanes do
        BPE.role(
          id:    :proplists.get_value(:id, att, []),
          tasks: for({_, [], {:value, name}} <- tasks_xml, do: name),
          name:  :unicode.characters_to_binary(:proplists.get_value(:name, att, []), :utf16)
        )
      end
    reduce(t, BPE.process(proc, roles: roles))
  end

  def reduce([{skip_type, _body, _attrs} | t], BPE.process() = proc)
      when skip_type in [:"bpmn:dataObjectReference", :"bpmn:dataObject",
                         :"bpmn:association", :"bpmn:textAnnotation",
                         :"bpmn:extensionElements"] do
    reduce(t, proc)
  end

  def fill_in_out(tasks, []), do: tasks

  def fill_in_out(tasks, [BPE.sequenceFlow(id: name, source: source, target: target) | flows]) do
    tasks1 = BPE.Gateways.key_push_value(name, BPE.Gateways.output_pos(), source, tasks)
    tasks2 = BPE.Gateways.key_push_value(name, BPE.Gateways.input_pos(),  target, tasks1)
    fill_in_out(tasks2, flows)
  end

  def fix_roles(tasks, []), do: tasks

  def fix_roles(tasks, [BPE.role(id: id, tasks: xml_tasks) | lanes]) do
    fix_roles(update_roles(xml_tasks, tasks, id), lanes)
  end

  def update_roles([], all_tasks, _role), do: all_tasks

  def update_roles([task_id | rest], all_tasks, role) do
    update_roles(rest, BPE.Gateways.key_push_value(role, BPE.Gateways.roles_pos(), task_id, all_tasks), role)
  end

  def action({:request, _, _}, p), do: {:reply, p}
  def auth(_), do: true

  def parse(term) when is_binary(term), do: parse(:erlang.binary_to_list(term))

  def parse(term) do
    {:ok, tokens, _} = :erl_scan.string(term)
    {:ok, abs_form}  = :erl_parse.parse_exprs(tokens)
    {_, value, _}    = :erl_eval.exprs(abs_form, :erl_eval.new_bindings())
    value
  end

  defp parse_expression(BPE.sequenceFlow(callbacks: c) = flow, [x | t]) when is_tuple(x) do
    case expression_type(x) do
      :callback  -> parse_expression(BPE.sequenceFlow(flow, callbacks: c ++ [x]), t)
      :condition -> parse_expression(BPE.sequenceFlow(flow, condition: x), t)
      _          -> parse_expression(BPE.sequenceFlow(flow, expression: x), t)
    end
  end

  defp parse_expression(flow, x) when is_tuple(x), do: parse_expression(flow, [x])
  defp parse_expression(flow, _), do: flow

  defp expression_type(x) when is_tuple(x) do
    case elem(x, 0) do
      :callback -> :callback
      t ->
        condition_types = [:service, :compare]
        if t in condition_types, do: :condition, else: :unknown
    end
  end

  defp expression_type(_), do: :unknown
end
