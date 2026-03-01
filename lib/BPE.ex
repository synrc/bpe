defmodule BPE do
  require Record

  defmacro xml(source) do
    mod = __CALLER__.module
    file = Mix.Project.app_path() <> source
    proc = Macro.escape(BPE.XML.load(to_charlist(file), mod))
    quote do: unquote(proc)
  end

  for {name, fields} <- Record.extract_all(from_lib: "bpe/include/bpe.hrl") do
    Record.defrecord(name, fields)
  end

  for {name, fields} <- Record.extract_all(from_lib: "bpe/include/doc.hrl") do
    Record.defrecord(name, fields)
  end

  for {name, fields} <- Record.extract_all(from_lib: "kvs/include/kvs.hrl") do
    Record.defrecord(name, fields)
  end

  for {name, fields} <- Record.extract_all(from_lib: "kvs/include/metainfo.hrl") do
    Record.defrecord(name, fields)
  end

  # ── private helpers ────────────────────────────────────────────────────────

  defp shutdown_timeout, do: :application.get_env(:bpe, :shutdown_timeout, 5_000)
  defp call_timeout,     do: :application.get_env(:bpe, :timeout, 6_000)
  defp driver,           do: :application.get_env(:bpe, :driver, :exclusive)

  defp bpe_id(record_name) do
    case :application.get_env(:kvs, :dba_seq, :kvs_rocks) do
      :kvs_rocks -> :kvs.seq([], [])
      _          -> :kvs.seq(record_name, 1)
    end
  end

  # Private helper: get sched record(s) for a process
  defp proc_sched(process(id: proc_id)), do: :kvs.all(key("/bpe/flow/", proc_id))
  defp proc_sched(proc_id) when is_binary(proc_id) or is_list(proc_id),
    do: :kvs.all(key("/bpe/flow/", proc_id))

  defp proc_sched(step(proc: proc_id) = s) do
    k =
      case :application.get_env(:kvs, :dba, :kvs_mnesia) do
        :kvs_rocks  -> key("/bpe/flow/", proc_id)
        :kvs_mnesia -> :sched
      end
    case :kvs.get(k, s) do
      {:ok, x} -> x
      _        -> []
    end
  end

  # ── key helpers ────────────────────────────────────────────────────────────

  def key({:step, n, [208 | _] = pid}), do: {:step, n, :erlang.list_to_binary(pid)}
  def key(pid), do: pid

  def key(prefix, pid), do: :erlang.iolist_to_binary([prefix, pid, "/"])

  # ── load / cleanup ─────────────────────────────────────────────────────────

  def load(id), do: load(id, [])

  def load(id, default) do
    case :application.get_env(:kvs, :dba, :kvs_mnesia) do
      :kvs_mnesia ->
        case :kvs.get(:process, id) do
          {:ok, p}    -> p
          {:error, _} -> default
        end
      :kvs_rocks ->
        case :kvs.get("/bpe/proc", id) do
          {:ok, p}    -> p
          {:error, _} -> default
        end
    end
  end

  def cleanup(p) do
    for hist(id: id) <- proc_hist(process(p, :id)), do: :kvs.delete("/bpe/hist", id)
    :kvs.delete(:writer, key("/bpe/hist/", p))
    for sched(id: id) <- proc_sched(p), do: :kvs.delete("/bpe/flow", id)
    :kvs.delete(:writer, key("/bpe/flow/", p))
    :kvs.delete("/bpe/proc", p)
  end

  def delete(process(id: pid, parent: parent, monitor: mid) = proc) do
    gw_unblock(pid)
    unsubscribe(pid, parent)
    :kvs.remove(proc, "/bpe/proc")
    case :kvs.get(key("/bpe/mon/", mid), pid) do
      {:ok, x} -> :kvs.remove(x, key("/bpe/mon/", mid))
      _        -> []
    end
    :kvs.append(process(proc, status: "deleted"), "/bpe/deleted")
    process(proc, status: "deleted")
  end

  # ── current task ───────────────────────────────────────────────────────────

  def current_task(process(id: id) = proc) do
    case head(id) do
      []                                                       -> {:empty, first_task(proc)}
      hist(id: {:step, h, _}, task: sequenceFlow(target: t))  -> {h, t}
      hist(id: {:step, h, _}, task: t)                        -> {h, t}
    end
  end

  # ── trace / history ────────────────────────────────────────────────────────

  def add_trace(proc, name, task) do
    add_hist(key("/bpe/hist/", process(proc, :id)), proc, name, task)
  end

  def add_error(proc, name, task) do
    :logger.notice(~c"BPE: Error for PID ~ts: ~p ~p", [process(proc, :id), name, task])
    add_hist(key("/bpe/error/", process(proc, :id)), proc, name, task)
  end

  def add_hist(k, process(executors: executors) = proc, name, task) do
    w = :kvs.writer(k)
    h = hist(
      id:        key({:step, writer(w, :count), process(proc, :id)}),
      name:      name,
      time:      ts(time: :calendar.local_time()),
      docs:      process(proc, :docs),
      task:      task,
      executors: executors
    )
    :kvs.append(h, k)
    h
  end

  def add_sched(proc, pointer, state) do
    k = key("/bpe/flow/", process(proc, :id))
    w = :kvs.writer(k)
    :kvs.append(
      sched(
        id:      key({:step, writer(w, :count), process(proc, :id)}),
        pointer: pointer,
        state:   state
      ),
      k
    )
  end

  # ── start / monitors ───────────────────────────────────────────────────────

  def start(process(docs: docs) = proc, []) do
    start(proc, docs, {[], procRec()})
  end

  def start(proc, options) do
    start(proc, options, {[], procRec()})
  end

  def start(proc0, options, {monitor, proc_rec}) do
    id =
      :erlang.iolist_to_binary([
        case process(proc0, :id) do
          [] -> bpe_id(:process)
          x  -> x
        end
      ])

    {hist_val, task} = current_task(process(proc0, id: id))
    pid_notif = :proplists.get_value(:notification, options, :undefined)

    s_proc =
      process(proc0,
        id:            id,
        docs:          options,
        notifications: pid_notif,
        modified:      ts(time: :calendar.local_time()),
        started:       ts(time: :calendar.local_time())
      )

    proc =
      case hist_val do
        :empty ->
          hist(task: stage) = add_trace(s_proc, [], task)
          add_sched(s_proc, 1, [first_flow(s_proc)])
          process(s_proc, stage: stage)
        _ -> s_proc
      end

    child_spec = {id, {BPE.Proc, :start_link, [proc]}, :transient, shutdown_timeout(), :worker, [BPE.Proc]}

    case cache(:terminateLocks, {:terminate, id}) do
      p when is_pid(p) ->
        mon = :erlang.monitor(:process, p)
        receive do
          {:DOWN, ^mon, :process, ^p, _} ->
            receive do {:EXIT, ^p, _r} -> :ok after 10 -> :shutdown end
            :erlang.unlink(p)
            :erlang.demonitor(mon)
        after shutdown_timeout() ->
          :logger.error(~c"BPE SHUTDOWN TIMEOUT: ~tp", [id])
          :erlang.unlink(p)
          :erlang.demonitor(mon)
        end
      _ -> []
    end

    case :supervisor.start_child(BPE.OTP, child_spec) do
      {:ok, _}                   -> mon_link(monitor, proc, proc_rec); {:ok, id}
      {:ok, _, _}                -> mon_link(monitor, proc, proc_rec); {:ok, id}
      {:error, :already_present} -> :supervisor.restart_child(BPE.OTP, id); {:ok, id}
      {:error, reason}           -> {:error, reason}
    end
  end

  defp terminate_lock(pid) do
    id = :erlang.integer_to_binary(:erlang.unique_integer([:positive, :monotonic]))
    :kvs.put(terminateLock(id: id, pid: pid), kvs(mod: :kvs_mnesia))
    id
  end

  def mon_link(mon, proc, proc_rec), do: mon_link(mon, proc, proc_rec, false)

  def mon_link([], proc, _, _), do: :kvs.append(proc, "/bpe/proc")

  def mon_link(monitor(parent: []) = m, process(parentMonitor: pmid) = p, pr, e) when pmid != [] do
    mon_link(monitor(m, parent: pmid), p, pr, e)
  end

  def mon_link(monitor(id: mid, parent: pmid) = m, process(id: proc_id) = proc, proc_rec, embedded) do
    k = key("/bpe/mon/", mid)
    :kvs.append(m, "/bpe/monitors")
    update_parent_monitor(m)

    memo_proc =
      if embedded do
        proc
      else
        msg_id = terminate_lock(proc_id)
        :gen_server.call(pid(proc_id), {msg_id, :mon_link, mid})
      end

    :kvs.append(process(memo_proc, monitor: mid, parentMonitor: pmid), "/bpe/proc")
    :kvs.append(procRec(proc_rec, id: proc_id), k)
    p = process(memo_proc, monitor: mid, parentMonitor: pmid)

    if embedded do
      p
    else
      msg_id2 = terminate_lock(proc_id)
      :gen_server.call(pid(proc_id), {msg_id2, :set, p})
      p
    end
  end

  defp update_parent_monitor(monitor(parent: pmid, creator: c) = x) do
    cr = if c == [], do: "default", else: c
    case :kvs.get("/bpe/monitors", pmid) do
      {:ok, monitor()} -> :kvs.append(x, key(key("/bpe/submonitors/", pmid), cr))
      _                -> []
    end
  end

  def mon_children(mid), do: :kvs.all(key("/bpe/mon/", mid))

  def pid(id), do: cache(:processes, {:process, :erlang.iolist_to_binary([id])})

  def ensure_mon(process(monitor: [], id: id) = proc) do
    mon      = monitor(id: bpe_id(:monitor))
    proc_rec = procRec()
    {mon, mon_link(mon, proc, procRec(proc_rec, id: id), true)}
  end

  def ensure_mon(process(monitor: mid) = proc) do
    case :kvs.get("/bpe/monitors", mid) do
      {:error, x} -> throw({:error, x})
      {:ok, mon}  -> {mon, proc}
    end
  end

  # ── API: proc / update / persist ──────────────────────────────────────────

  def proc(proc_id) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :get}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def update(proc_id, state) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :set, state}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def update(proc_id, state, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :set, state, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def persist(proc_id, state) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :persist, state}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def persist(proc_id, state, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :persist, state, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  # ── assign ─────────────────────────────────────────────────────────────────

  def assign(proc_id) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :ensure_mon}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def assign(proc_id, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :ensure_mon, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  # ── complete ───────────────────────────────────────────────────────────────

  def complete(proc_id) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :complete}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def complete(proc_id, [continue() | _] = cont) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :complete, cont}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def complete(proc_id, stage) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :complete, stage}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def complete(proc_id, stage, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :complete, stage, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  # ── next ───────────────────────────────────────────────────────────────────

  def next(proc_id) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :next}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def next(proc_id, [continue() | _] = cont) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :next, cont}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def next(proc_id, stage) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :next, stage}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def next(proc_id, stage, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :next, stage, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  # ── amend / discard / modify ───────────────────────────────────────────────

  def amend(proc_id, form) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :amend, form}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def amend(proc_id, form, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :amend, form, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def discard(proc_id, form) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :discard, form}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def discard(proc_id, form, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :discard, form, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def modify(proc_id, form, arg) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :modify, form, arg}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def modify(proc_id, form, arg, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :modify, form, arg, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  # ── events ─────────────────────────────────────────────────────────────────

  def message_event(proc_id, event) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :messageEvent, event}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def message_event(proc_id, event, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.call(pid(proc_id), {id, :messageEvent, event, continue}, call_timeout())
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def async_event(proc_id, event) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.cast(pid(proc_id), {id, :asyncEvent, event})
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def async_event(proc_id, event, continue) do
    id = terminate_lock(proc_id)
    start(load(proc_id), [])
    try do
      :gen_server.cast(pid(proc_id), {id, :asyncEvent, event, continue})
    catch
      :exit, {:normal, _} -> {:exit, :normal}
      _, z                -> {:error, z}
    end
  end

  def broadcast_event(topic, broadcastEvent(type: :immediate) = ev) do
    Enum.each(
      :kvs.index(:subscription, :topic, topic, kvs(mod: :kvs_mnesia)),
      fn subscription(who: sub_pid) ->
        id = terminate_lock(sub_pid)
        start(load(sub_pid), [])
        try do
          :gen_server.cast(pid(sub_pid), {id, :broadcastEvent,
            broadcastEvent(ev, id: :kvs.seq([], []), topic: topic)})
        catch
          :exit, {:normal, _} -> {:exit, :normal}
          _, z                -> {:error, z}
        end
      end
    )
  end

  def broadcast_event(topic, broadcastEvent() = ev) do
    Enum.each(
      :kvs.index(:subscription, :topic, topic, kvs(mod: :kvs_mnesia)),
      fn subscription(who: sub_pid) ->
        :kvs.append(
          broadcastEvent(ev, id: :kvs.seq([], []), topic: topic),
          key("/bpe/messages/queue/", sub_pid)
        )
      end
    )
  end

  # ── gateway block / subscribe ──────────────────────────────────────────────

  def gw_block(bpe_pid, gw, subject) do
    case :kvs.index_match(gw_block(id: :_, pid: bpe_pid, subject: subject, gw: gw), :pid, kvs(mod: :kvs_mnesia)) do
      [] -> :kvs.put(gw_block(id: :kvs.seq([], []), pid: bpe_pid, subject: subject, gw: gw), kvs(mod: :kvs_mnesia))
      _  -> :exist
    end
  end

  def gw_unblock(bpe_pid) do
    :kvs.index_match(gw_block(id: :_, pid: bpe_pid, subject: :_, gw: :_), :pid, kvs(mod: :kvs_mnesia))
    |> Enum.each(fn gw_block(id: id) -> :kvs.delete(:gw_block, id, kvs(mod: :kvs_mnesia)) end)
  end

  def gw_unblock(bpe_pid, gw, subject) do
    :kvs.index_match(gw_block(id: :_, pid: bpe_pid, subject: subject, gw: gw), :subject, kvs(mod: :kvs_mnesia))
    |> Enum.each(fn gw_block(id: id) -> :kvs.delete(:gw_block, id, kvs(mod: :kvs_mnesia)) end)
  end

  def subscribe(bpe_pid, topic) do
    case :kvs.index_match(subscription(id: :_, who: bpe_pid, topic: topic), :who, kvs(mod: :kvs_mnesia)) do
      [] -> :kvs.put(subscription(id: :kvs.seq([], []), who: bpe_pid, topic: topic), kvs(mod: :kvs_mnesia))
      _  -> :exist
    end
  end

  def unsubscribe(bpe_pid, topic) do
    :kvs.index_match(subscription(id: :_, who: bpe_pid, topic: topic), :who, kvs(mod: :kvs_mnesia))
    |> Enum.each(fn subscription(id: id) -> :kvs.delete(:subscription, id, kvs(mod: :kvs_mnesia)) end)
  end

  # ── syn helpers ────────────────────────────────────────────────────────────

  def send(pool, message), do: :syn.publish(:erlang.term_to_binary(pool), message)

  def reg(pool), do: reg(pool, :undefined)

  def reg(pool, _value) do
    case :erlang.get({:pool, pool}) do
      :undefined ->
        :syn.join(:erlang.term_to_binary(pool), self())
        :erlang.put({:pool, pool}, pool)
      _ -> :skip
    end
  end

  def unreg(pool) do
    case :erlang.get({:pool, pool}) do
      :undefined -> :skip
      _ ->
        :syn.leave(pool, self())
        :erlang.erase({:pool, pool})
    end
  end

  # ── flow / task helpers ────────────────────────────────────────────────────

  def first_flow(process(beginEvent: begin_event, flows: flows)) do
    f = Enum.find(flows, fn f -> sequenceFlow(f, :source) == begin_event end)
    sequenceFlow(f, :id)
  end

  def first_task(process(tasks: tasks)) do
    case for beginEvent(id: n) <- tasks, do: n do
      []         -> []
      [name | _] -> name
    end
  end

  # ── head / hist / sched ────────────────────────────────────────────────────

  def head(proc_id) do
    k =
      case :application.get_env(:kvs, :dba, :kvs_mnesia) do
        :kvs_rocks  -> key("/bpe/hist/", proc_id)
        :kvs_mnesia -> :hist
      end
    case :kvs.get(:writer, key("/bpe/hist/", proc_id)) do
      {:ok, w} ->
        case :kvs.get(k, key({:step, writer(w, :count) - 1, proc_id})) do
          {:ok, x} -> x
          _        -> []
        end
      _ -> []
    end
  end

  def sched_head(proc_id) do
    k =
      case :application.get_env(:kvs, :dba, :kvs_mnesia) do
        :kvs_rocks  -> key("/bpe/flow/", proc_id)
        :kvs_mnesia -> :sched
      end
    case :kvs.get(:writer, key("/bpe/flow/", proc_id)) do
      {:ok, w} ->
        case :kvs.get(k, key({:step, writer(w, :count) - 1, proc_id})) do
          {:ok, x} -> x
          _        -> []
        end
      _ -> []
    end
  end

  def errors(proc_id), do: :kvs.all(key("/bpe/error/", proc_id))

  # renamed from hist/1, hist/2 to avoid clash with hist record macro
  def proc_hist(step(proc: proc_id, id: n)), do: proc_hist(proc_id, n)
  def proc_hist(proc_id), do: :kvs.all(key("/bpe/hist/", proc_id))

  def proc_hist(proc_id, n) do
    k =
      case :application.get_env(:kvs, :dba, :kvs_mnesia) do
        :kvs_rocks  -> key("/bpe/hist/", proc_id)
        :kvs_mnesia -> :hist
      end
    case :kvs.get(k, key({:step, n, proc_id})) do
      {:ok, res}  -> res
      {:error, _} -> []
    end
  end

  # renamed from step/2 to avoid clash with step record macro
  def find_step(proc, name) do
    case for t <- tasks(proc), elem(t, 1) == name, do: t do
      [t] -> t
      []  -> task()
      e   -> e
    end
  end

  def docs(proc) do
    h = head(process(proc, :id))
    hist(h, :docs)
  end

  def tasks(proc),  do: process(proc, :tasks)
  def flows(proc),  do: process(proc, :flows)
  def events(proc), do: process(proc, :events)

  def doc(r, proc) do
    {x, _} = BPE.Env.find(:env, proc, r)
    x
  end

  def flow(flow_id, process(flows: flows)) do
    Enum.find(flows, fn f -> sequenceFlow(f, :id) == flow_id end) || false
  end

  def flow_id(sched(state: flows, pointer: n)), do: Enum.at(flows, n - 1)

  # ── cache ──────────────────────────────────────────────────────────────────

  def cache(table, key_val, :undefined), do: :ets.delete(table, key_val)

  def cache(table, key_val, value) do
    :ets.insert(table, {key_val, till(:calendar.local_time(), ttl()), value})
    value
  end

  def cache(table, key_val, value, till_val) do
    :ets.insert(table, {key_val, till_val, value})
    value
  end

  def cache(table, key_val) do
    val =
      case :ets.lookup(table, key_val) do
        []  -> :undefined
        [v] -> v
        vs  -> vs
      end
    case val do
      :undefined         -> :undefined
      {_, :infinity, x}  -> x
      {_, expire, x}     ->
        if expire < :calendar.local_time() do
          :ets.delete(table, key_val)
          :undefined
        else
          x
        end
    end
  end

  def ttl, do: :application.get_env(:bpe, :ttl, 60 * 15)

  def till(now, ttl_val) do
    if is_atom(ttl_val) do
      ttl_val
    else
      :calendar.gregorian_seconds_to_datetime(
        :calendar.datetime_to_gregorian_seconds(now) + ttl_val
      )
    end
  end

  def reload(module) do
    case :code.get_object_code(module) do
      :error                      -> {:load_error, module}
      {^module, binary, filename} ->
        case :code.load_binary(module, filename, binary) do
          {:module, ^module}  -> {:reloaded, module}
          {:error, reason}    -> {:load_error, module, reason}
        end
    end
  end

  # ── construct result ───────────────────────────────────────────────────────

  def construct_result(result(type: :reply, opt: [], reply: r, state: st)),
    do: {:reply, r, st}
  def construct_result(result(type: :reply, opt: o, reply: r, state: st)),
    do: {:reply, r, st, flatten_opt(o)}
  def construct_result(result(type: :noreply, opt: [], state: st)),
    do: {:noreply, st}
  def construct_result(result(type: :noreply, opt: opt, state: st)),
    do: {:noreply, st, flatten_opt(opt)}
  def construct_result(result(type: :stop, reply: [], reason: reason, state: st)),
    do: {:stop, reason, st}
  def construct_result(result(type: :stop, reply: reply, reason: reason, state: st)),
    do: {:stop, reason, reply, st}
  def construct_result(_),
    do: {:stop, :error, "Invalid return value", []}

  defp flatten_opt({:continue, c}), do: {:continue, List.flatten(c)}
  defp flatten_opt(x), do: x

  # ── process flow ───────────────────────────────────────────────────────────

  def process_flow(forced_flow_id, process() = proc) do
    case flow(forced_flow_id, proc) do
      false ->
        add_error(proc, "No such sequenceFlow", forced_flow_id)
        construct_result(result(type: :reply,
          reply: {:error, "No such sequenceFlow", forced_flow_id}, state: proc))
      forced_flow ->
        threads = sched_head(process(proc, :id)) |> sched(:state)
        case :string.str(threads, [forced_flow_id]) do
          0 ->
            add_error(proc, "Unavailable flow", forced_flow)
            construct_result(result(type: :reply,
              reply: {:error, "Unavailable flow", forced_flow}, state: proc))
          new_pointer ->
            add_sched(proc, new_pointer, threads)
            add_trace(proc, "Forced Flow", forced_flow)
            process_flow(proc)
        end
    end
  end

  def process_flow(process() = proc) do
    construct_result(process_sched(sched_head(process(proc, :id)), proc))
  end

  def process_sched(sched(state: []), proc) do
    result(type: :stop, reason: :normal, reply: :Final, state: proc)
  end

  def process_sched(sched() = s, proc) do
    f = flow(flow_id(s), proc)
    src = sequenceFlow(f, :source)
    dst = sequenceFlow(f, :target)
    # elem index 1 = id field (0-based), shared by all TASK-based records
    source_task = Enum.find(tasks(proc), fn t -> is_tuple(t) and elem(t, 1) == src end)
    target_task = Enum.find(tasks(proc), fn t -> is_tuple(t) and elem(t, 1) == dst end)
    module      = process(proc, :module)
    # elem index 6 = roles field (0-based): tuple is {rec, id, name, input, output, prompt, roles, ...}
    authorized  = apply(module, :auth, [elem(source_task, 6)])
    process_authorized(authorized, source_task, target_task, f, s, proc)
  end

  def process_authorized(false, source_task, _target_task, flow_rec, _sched_rec, proc) do
    add_error(proc, "Access denied", flow_rec)
    result(type: :reply, reply: {:error, "Access denied", source_task}, state: proc)
  end

  def process_authorized(true, _, target_task, flow_rec,
      sched(id: sched_id, pointer: pointer, state: threads), proc) do
    sequenceFlow(id: next, source: src, target: dst) = flow_rec
    result(state: state, reason: reason, type: status, executed: executed) = res =
      BPE.Task.task_action(process(proc, :module), src, dst, proc)

    inserted    = get_inserted(target_task, flow_rec, sched_id, state)
    new_threads = Enum.slice(threads, 0, pointer - 1) ++ inserted ++ Enum.drop(threads, pointer)
    new_pointer = if pointer == length(threads), do: 1, else: pointer + length(inserted)
    new_executed = add_executed(proc, executed)

    add_sched(state, new_pointer, new_threads)
    new_state0  = process(state, executors: executors_for(state, flow_rec))
    new_result0 = result(res, state: new_state0, executed: new_executed)
    hist(task: new_task) = add_trace(new_state0, [], flow_rec)
    new_state   = process(new_state0, stage: new_task)
    new_result  = result(new_result0, state: new_state)
    BPE.Proc.debug(new_state, next, src, dst, status, reason)
    :kvs.append(new_state, "/bpe/proc")
    flow_callback(flow_rec, new_result, proc)
    new_result
  end

  defp flow_callback(sequenceFlow(source: src, target: tgt, callbacks: [{:callback, fun} | t]) = f, res, process(module: mod) = prev) do
    flow_callback(sequenceFlow(f, callbacks: t), apply(mod, fun, [{:callback, src, tgt}, res, prev]), prev)
  end

  defp flow_callback(sequenceFlow(source: src, target: tgt, callbacks: [{:callback, fun, mod} | t]) = f, res, prev) do
    flow_callback(sequenceFlow(f, callbacks: t), apply(mod, fun, [{:callback, src, tgt}, res, prev]), prev)
  end

  defp flow_callback(sequenceFlow(source: src, target: tgt, callbacks: [{:callback, fun, mod, arg} | t]) = f, res, prev) do
    flow_callback(sequenceFlow(f, callbacks: t), apply(mod, fun, [{:callback, src, tgt}, res, arg]), prev)
  end

  defp flow_callback(sequenceFlow(callbacks: []), r, _), do: r
  defp flow_callback(_, r, _), do: r

  defp add_executed(process(id: id, executors: prev_executors), executed0) do
    k    = key("/bpe/hist/", id)
    w    = :kvs.writer(k)
    time = :calendar.local_time()

    executed =
      Enum.map(executed0, fn
        executor(executed: []) = x -> executor(x, executed: ts(time: time))
        x -> x
      end)

    new_executed =
      Enum.map(prev_executors, fn executor(id: eid, executed: e) = r ->
        case Enum.find(executed, fn executor(id: i) -> i == eid end) do
          executor(executed: x) when e == [] -> executor(r, executed: x)
          _ -> r
        end
      end)

    case :kvs.get(k, key({:step, writer(w, :count) - 1, id})) do
      {:error, _}      -> []
      {:ok, hist() = h} -> :kvs.append(hist(h, executors: new_executed), k)
    end

    executed
  end

  defp executors_for(process(executors: e),
      sequenceFlow(source: s, target: t, expression: {:save_executors, task_name})) do
    if s == task_name or t == task_name, do: e, else: []
  end

  defp executors_for(process(module: module) = state, sequenceFlow(source: s, target: t)) do
    if :erlang.function_exported(module, :executors, 2) do
      handle_executors(apply(module, :executors, [{:request, s, t}, state]))
    else
      []
    end
  end

  defp handle_executors(execs) do
    Enum.map(execs, fn executor() = r -> executor(r, received: ts(time: :calendar.local_time())) end)
  end

  # ── get_inserted ───────────────────────────────────────────────────────────

  # output field is at 1-based tuple position 5, 0-based index 4
  def get_inserted(t, flow_rec, sched_id, proc) do
    if :erlang.element(5, t) == [] do
      []
    else
      do_get_inserted({t, flow_rec, sched_id, proc})
    end
  end

  defp do_get_inserted({gateway(type: :exclusive, output: out, def: []) = gw, _, _, proc}) do
    case first_matched_flow(out, proc) do
      [] ->
        add_error(proc, "All conditions evaluate to false in exclusive gateway without default",
          gateway(gw, :id))
        []
      x -> x
    end
  end

  defp do_get_inserted({gateway(type: :exclusive, output: out, def: def_flow), _, _, proc}) do
    case first_matched_flow(out -- [def_flow], proc) do
      [] -> [def_flow]
      x  -> x
    end
  end

  defp do_get_inserted({gateway(type: type, input: in_flows, output: out), flow_rec, sched_id, _proc})
       when type in [:inclusive, :parallel] do
    case check_all_flows(in_flows -- [sequenceFlow(flow_rec, :id)], sched_id) do
      true  -> out
      false -> []
    end
  end

  defp do_get_inserted({t, _, _, proc}), do: apply(__MODULE__, driver(), [t, proc])

  def exclusive(t, proc),  do: first_matched_flow(:erlang.element(5, t), proc)
  def last(t, _proc),      do: [List.last(:erlang.element(5, t))]
  def first(t, _proc),     do: [hd(:erlang.element(5, t))]

  def random(t, _proc) do
    out = :erlang.element(5, t)
    [Enum.at(out, :rand.uniform(length(out)) - 1)]
  end

  def check_all_flows([], _), do: true
  def check_all_flows(_, step(id: 0)), do: false

  def check_all_flows(needed, step(id: id) = sched_id) do
    case proc_hist(sched_id) do
      hist(task: sequenceFlow(id: fid)) ->
        check_all_flows(needed -- [fid], step(sched_id, id: id - 1))
      _ -> false
    end
  end

  def first_matched_flow([], _proc), do: []

  def first_matched_flow([h | flows], proc) do
    case check_flow_condition(flow(h, proc), proc) do
      true  -> [h]
      false -> first_matched_flow(flows, proc)
    end
  end

  def check_flow_condition(sequenceFlow(condition: {:compare, bpe_doc_param, field, const}), proc) do
    case doc(bpe_doc_param, proc) do
      [] ->
        add_error(proc, "No such document", bpe_doc_param)
        false
      docs_list when is_list(docs_list) ->
        # field is 1-based (Erlang convention); convert to 0-based for elem/2
        elem(hd(docs_list), field - 1) == const
    end
  end

  def check_flow_condition(sequenceFlow(source: gw, condition: {:service, :gw_block}), process(id: bpe_pid)) do
    :kvs.index_match(gw_block(id: :_, gw: gw, subject: bpe_pid, pid: :_), :subject, kvs(mod: :kvs_mnesia)) == []
  end

  def check_flow_condition(sequenceFlow(condition: {:service, fun}), process(module: module) = proc) do
    apply(module, fun, [proc])
  end

  def check_flow_condition(sequenceFlow(condition: {:service, fun, mod}), proc) do
    apply(mod, fun, [proc])
  end

  def check_flow_condition(sequenceFlow(condition: []), _proc), do: true
end
