defmodule BPE.Gateways do
  require BPE
  import BPE

  # field positions in the task/gateway tuple (1-based, as used by :erlang.element/2)
  # tuple layout: {record_atom, id, name, input, output, prompt, roles, etc, ...}
  @id_pos     2
  @input_pos  4
  @output_pos 5
  @roles_pos  7

  def def do
    r_raw = [
      {:r1, :a, :b}, {:r2, :b, :c},  {:r3, :b, :d},  {:r4, :c, :e},
      {:r5, :c, :f}, {:r6, :d, :g},  {:r7, :e, :h},  {:r8, :h, :j},
      {:r9, :f, :i}, {:r10, :i, :j}, {:r11, :f, :g}, {:r12, :g, :j},
      {:r13, :j, :k}
    ]
    {r, n} = get_edges_and_vertices(r_raw)
    p = process(name: :Graph, flows: r, tasks: n, beginEvent: :a, endEvent: :k, events: [])
    fix_gateways(fix_begin_end_events(p))
  end

  def vertices(r), do: vertices(Enum.reverse(r), [])
  def vertices([], n), do: n

  def vertices([sequenceFlow(id: id, source: from, target: to) | rtail], n) do
    n1 =
      case find_task(from, n) do
        false -> [task(name: from, output: [id]) | n]
        v     -> replace_task(from, n, task(v, output: [id | task(v, :output)]))
      end
    new_n =
      case find_task(to, n1) do
        false -> [task(name: to, input: [id]) | n1]
        v1    -> replace_task(to, n1, task(v1, input: [id | task(v1, :input)]))
      end
    vertices(rtail, new_n)
  end

  def get_edges_and_vertices(edges_raw) do
    r = for {id, from, to} <- edges_raw, do: sequenceFlow(name: id, source: from, target: to)
    {r, vertices(r)}
  end

  def set_vertex_type(id, type, vertices) do
    case find_task(id, vertices) do
      false -> vertices
      v     ->
        new_vertex = gateway(id: task(v, :id), name: task(v, :name),
                             input: task(v, :input), output: task(v, :output), type: type)
        replace_task(id, vertices, new_vertex)
    end
  end

  def fix_begin_end_events(proc) do
    tasks = process(proc, :tasks)
    [begin_task] = for task(input: []) = t <- tasks, do: t
    [end_task]   = for task(output: []) = t <- tasks, do: t
    begin_event  = beginEvent(name: task(begin_task, :name))
    end_event    = endEvent(name: task(end_task, :name))
    process(proc,
      tasks:       [begin_event, end_event | tasks -- [begin_task, end_task]],
      beginEvent:  task(begin_task, :name),
      endEvent:    task(end_task, :name)
    )
  end

  def fix_gateways(process(tasks: tasks) = proc) do
    process(proc, tasks: Enum.map(tasks, &convert_if_needed/1))
  end

  def convert_if_needed(task(name: name, input: in_f, output: out_f) = t)
      when length(in_f) > 1 or length(out_f) > 1 do
    gateway(name: name, type: :parallel, input: task(t, :input), output: task(t, :output))
  end

  def convert_if_needed(t), do: t

  def action({:request, _, _}, proc), do: {:reply, proc}

  # ── helpers ─────────────────────────────────────────────────────────────────

  defp find_task(id, list) do
    Enum.find(list, fn t -> is_tuple(t) and :erlang.element(@id_pos, t) == id end)
  end

  defp replace_task(id, list, new_val) do
    Enum.map(list, fn t ->
      if is_tuple(t) and :erlang.element(@id_pos, t) == id, do: new_val, else: t
    end)
  end

  # Called from BPE.XML.fill_in_out with field positions
  def key_push_value(value, value_key_pos, elem_id, list) do
    case find_task(elem_id, list) do
      nil   -> list
      false -> list
      found ->
        if elem(found, 0) == :endEvent do
          list
        else
          new_found = :erlang.setelement(value_key_pos, found,
            [value | :erlang.element(value_key_pos, found)])
          replace_task(elem_id, list, new_found)
        end
    end
  end

  def output_pos, do: @output_pos
  def input_pos,  do: @input_pos
  def roles_pos,  do: @roles_pos
  def id_pos,     do: @id_pos
end
