defmodule BPE.Proc do
  require BPE
  import BPE

  use GenServer

  defp request_limit, do: :application.get_env(:bpe, :request_limit, 500)

  def start_link(parameters), do: GenServer.start_link(__MODULE__, parameters, [])

  def debug(proc, name, targets, target, status, reason) do
    if :application.get_env(:bpe, :debug, true) == true do
      :logger.notice(~c"BPE: ~ts [~ts:~ts] ~p/~p ~p",
        [process(proc, :id), name, target, status, reason, targets])
    end
  end

  defp process_event(event_type, event, process(id: pid, module: module) = proc) do
    name = :kvs.field(event, :name)
    result(type: t, state: new_proc) = res =
      case apply(module, :action, [event, proc]) do
        result(type: :reply) = r when event_type == :async -> result(r, type: :noreply, reply: [])
        result() = r when event_type == :async             -> result(r, reply: [])
        r -> r
      end
    debug(proc, Atom.to_charlist(elem(event, 0)), "", name, t, "")
    :kvs.append(new_proc, "/bpe/proc")
    :kvs.remove(event, BPE.key("/bpe/messages/queue/", pid))
    :kvs.append(:erlang.setelement(2, event, :kvs.seq([], [])), BPE.key("/bpe/messages/hist/", pid))
    case res do
      result(continue: c) = x when c != [] -> BPE.construct_result(result(x, opt: {:continue, c}))
      x                                     -> BPE.construct_result(x)
    end
  end

  def process_task(stage, proc), do: process_task(stage, proc, false)

  def process_task(stage, proc, no_flow) do
    {_, curr} = BPE.current_task(proc)
    task_rec  = BPE.find_step(proc, curr)
    targets   = if no_flow, do: :noflow, else: BPE.Task.targets(curr, proc)

    {status, {reason, target}, proc_state} =
      case {targets, curr, stage} do
        {:noflow, _, _}  -> {:reply, {:complete, curr}, proc}
        {[], [], _}      -> BPE.Task.already_finished(proc)
        {[], ^curr, _}   -> BPE.Task.handle_task(task_rec, curr, curr, proc)
        {[], _, _}       -> BPE.Task.denied_flow(curr, proc)
        {list, _, []}    -> BPE.Task.handle_task(task_rec, curr, BPE.Task.find_flow(stage, list), proc)
        {list, _, _}     -> {:reply, {:complete, BPE.Task.find_flow(stage, list)}, proc}
      end

    if status == :stop or no_flow do
      {:stop, reason, target, proc_state}
    else
      BPE.add_trace(proc_state, [], target)
      debug(proc_state, curr, targets, target, status, reason)
      {status, {reason, target}, proc_state}
    end
  end

  # ── internal helpers ────────────────────────────────────────────────────────

  defp behavior_fun(:asyncEvent, _), do: :handle_cast
  defp behavior_fun(:broadcastEvent, {_, :broadcastEvent, broadcastEvent(type: :immediate)}), do: :handle_cast
  defp behavior_fun(_, _), do: :handle_call

  defp behavior_fun_args(:handle_call, req, from, st), do: [req, from, st]
  defp behavior_fun_args(_, req, _, st), do: [req, st]

  defp convert_api_args(:proc,   [id, _pid]),           do: {id, :get}
  defp convert_api_args(:update, [id, _pid, state]),    do: {id, :set, state}
  defp convert_api_args(:assign, [id, _pid]),           do: {id, :ensure_mon}
  defp convert_api_args(fn_name, [id, _ | args]),       do: List.to_tuple([id, fn_name | args])

  defp continue_id([continue() | _] = x, id) do
    Enum.map(x, fn c -> continue(c, id: id) end)
  end
  defp continue_id(_, _), do: []

  defp delete_lock(id), do: :kvs.delete(:terminateLock, id, kvs(mod: :kvs_mnesia))

  defp handle_result(:call, {:noreply, s}),    do: {:reply, :ok, s}
  defp handle_result(:call, {:noreply, s, x}), do: {:reply, :ok, s, x}
  defp handle_result(_, x),                    do: x

  defp handle_result(type, id, x, process(id: pid) = def_state) do
    handle_result(type, terminate_check(id, x,
      :kvs.index(:terminateLock, :pid, pid, kvs(mod: :kvs_mnesia)),
      request_limit(), def_state))
  end

  defp terminate_check(_, _, l, limit, process(id: pid) = def_state) when length(l) > limit do
    :logger.error(~c"TERMINATE LOCK LIMIT: ~tp", [pid])
    BPE.cache(:terminateLocks, {:terminate, pid}, self())
    {:stop, :normal, def_state}
  end

  defp terminate_check(id, x, [terminateLock(id: id)], _, process(id: pid)) do
    BPE.cache(:terminateLocks, {:terminate, pid},
      if(elem(x, 0) == :stop, do: self(), else: :undefined))
    delete_lock(id)
    x
  end

  defp terminate_check(id, {:stop, :normal, s}, [terminateLock() | _], _, _) do
    delete_lock(id)
    {:noreply, s}
  end

  defp terminate_check(id, {:stop, :normal, reply, s}, [terminateLock() | _], _, _) do
    delete_lock(id)
    {:reply, reply, s}
  end

  defp terminate_check(id, x, _, _, process(id: pid)) do
    BPE.cache(:terminateLocks, {:terminate, pid},
      if(elem(x, 0) == :stop, do: self(), else: :undefined))
    delete_lock(id)
    x
  end

  # Wraps a gen_server result with continuation support (renamed to avoid clash with OTP callback)
  defp extend_continue({:noreply, state}, cont, id) when cont != [] do
    {:noreply, state, {:continue, continue_id(List.flatten(cont), id)}}
  end

  defp extend_continue({:reply, reply, state, {:continue, c}}, cont, id) do
    {:reply, reply, state, {:continue, continue_id(List.flatten(cont) ++ c, id)}}
  end

  defp extend_continue({:reply, reply, state, _}, cont, id) when cont != [] do
    {:reply, reply, state, {:continue, continue_id(List.flatten(cont), id)}}
  end

  defp extend_continue({:noreply, state, {:continue, c}}, cont, id) do
    {:noreply, state, {:continue, continue_id(List.flatten(cont) ++ c, id)}}
  end

  defp extend_continue({:noreply, state, _}, cont, id) when cont != [] do
    {:noreply, state, {:continue, continue_id(List.flatten(cont), id)}}
  end

  defp extend_continue({:reply, reply, state}, cont, id) when cont != [] do
    {:reply, reply, state, {:continue, continue_id(List.flatten(cont), id)}}
  end

  defp extend_continue({:stop, _, state}, cont, id) when cont != [] do
    {:noreply, state, {:continue, continue_id(List.flatten(cont) ++ [continue(type: :stop)], id)}}
  end

  defp extend_continue({:stop, _, _reply, state}, cont, id) when cont != [] do
    {:noreply, state, {:continue, continue_id(List.flatten(cont) ++ [continue(type: :stop)], id)}}
  end

  defp extend_continue(x, _, _), do: x

  # ── GenServer callbacks ─────────────────────────────────────────────────────

  @impl true
  def init(process() = p) do
    :erlang.process_flag(:trap_exit, true)
    BPE.cache(:terminateLocks, {:terminate, process(p, :id)}, :undefined)
    proc = BPE.load(process(p, :id), p)
    :logger.notice(~c"BPE: ~ts spawned as ~p", [process(proc, :id), self()])
    till = BPE.till(:calendar.local_time(), :application.get_env(:bpe, :ttl, 24 * 60 * 60))
    BPE.cache(:processes, {:process, process(proc, :id)}, self(), till)
    for event_rec <- BPE.events(proc) do
      BPE.reg({:messageEvent, elem(event_rec, 0), process(proc, :id)})
    end
    {:ok, process(proc, timer: :erlang.send_after(:rand.uniform(10_000), self(), {:timer, :ping}))}
  end

  @impl true
  def handle_call({:stop, reason}, _, proc), do: {:stop, reason, proc}

  def handle_call({id, :mon_link, mid}, _, proc) do
    :logger.notice(~c"BPE MON_LINK: ~p.", [process(proc, :id)])
    new_proc = process(proc, monitor: mid)
    handle_result(:call, id, {:reply, new_proc, new_proc}, proc)
  end

  def handle_call({id, :ensure_mon}, _, proc) do
    :logger.notice(~c"BPE ENSURE_MON: ~p.", [process(proc, :id)])
    {mon, new_proc} = BPE.ensure_mon(proc)
    handle_result(:call, id, {:stop, :normal, mon, new_proc}, proc)
  end

  def handle_call({id, :get}, _, proc) do
    :logger.notice(~c"BPE GET: ~p.", [process(proc, :id)])
    handle_result(:call, id, {:stop, :normal, proc, proc}, proc)
  end

  def handle_call({id, :set, state}, _, proc) do
    :logger.notice(~c"BPE SET: ~p.", [process(proc, :id)])
    handle_result(:call, id, {:stop, :normal, proc, state}, state)
  end

  def handle_call({id, :persist, state}, _, process() = proc) do
    :logger.notice(~c"BPE PERSIST: ~p.", [process(proc, :id)])
    :kvs.append(state, "/bpe/proc")
    handle_result(:call, id, {:stop, :normal, state, state}, state)
  end

  def handle_call({id, :next}, _, process() = proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(proc), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"next/1", z}, {:error, :"next/1", z}, proc}
    end
  end

  def handle_call({id, :next, [continue() | _] = cont}, _, process() = proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(proc), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"next/1", z}, {:error, :"next/1"}, proc}
    end
  end

  def handle_call({id, :next, stage}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(stage, proc), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"next/2", z}, {:error, :"next/2", z}, proc}
    end
  end

  def handle_call({id, :amend, form}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(BPE.Env.append(:env, proc, form)), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"amend/2", z}, {:error, :"amend/2", z}, proc}
    end
  end

  def handle_call({id, :discard, form}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(BPE.Env.remove(:env, proc, form)), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"amend/2", z}, {:error, :"discard/2", z}, proc}
    end
  end

  def handle_call({id, :messageEvent, event}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_event(:sync, event, proc), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"messageEvent/2", z}, {:error, :"messageEvent/2", z}, proc}
    end
  end

  def handle_call({id, :messageEvent, event, [continue() | _] = cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_event(:sync, event, proc), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"messageEvent/3", z}, {:error, :"messageEvent/3", z}, proc}
    end
  end

  def handle_call({id, :complete}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task([], proc), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"complete/1", z}, {:error, :"complete/1", z}, proc}
    end
  end

  def handle_call({id, :complete, [continue() | _] = cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task([], proc), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"complete/2", z}, {:error, :"complete/2", z}, proc}
    end
  end

  def handle_call({id, :complete, stage}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task(stage, proc), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"complete/2", z}, {:error, :"complete/2", z}, proc}
    end
  end

  def handle_call({id, :modify, form, :append}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task([], BPE.Env.append(:env, proc, form), true), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"append/2", z}, {:error, :"append/2", z}, proc}
    end
  end

  def handle_call({id, :modify, form, :remove}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task([], BPE.Env.remove(:env, proc, form), true), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"remove/2", z}, {:error, :"remove/2", z}, proc}
    end
  end

  def handle_call({id, :mon_link, mid, cont}, _, proc) do
    :logger.notice(~c"BPE MON_LINK CONTINUE: ~p.", [process(proc, :id)])
    new_proc = process(proc, monitor: mid)
    handle_result(:call, id, extend_continue({:stop, :normal, new_proc, new_proc}, cont, id), proc)
  end

  def handle_call({id, :ensure_mon, cont}, _, proc) do
    :logger.notice(~c"BPE ENSURE_MON CONTINUE: ~p.", [process(proc, :id)])
    {mon, new_proc} = BPE.ensure_mon(proc)
    handle_result(:call, id, extend_continue({:stop, :normal, mon, new_proc}, cont, id), proc)
  end

  def handle_call({id, :set, state, cont}, _, proc) do
    :logger.notice(~c"BPE SET CONTINUE: ~p.", [process(proc, :id)])
    handle_result(:call, id, extend_continue({:stop, :normal, proc, state}, cont, id), state)
  end

  def handle_call({id, :persist, state, cont}, _, process() = proc) do
    :logger.notice(~c"BPE PERSIST CONTINUE: ~p.", [process(proc, :id)])
    :kvs.append(state, "/bpe/proc")
    handle_result(:call, id, extend_continue({:stop, :normal, state, state}, cont, id), state)
  end

  def handle_call({id, :next, stage, cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(stage, proc), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"next/2", z}, {:error, :"next/2", z}, proc}
    end
  end

  def handle_call({id, :amend, form, cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(BPE.Env.append(:env, proc, form)), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"amend/2", z}, {:error, :"amend/2", z}, proc}
    end
  end

  def handle_call({id, :discard, form, cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(BPE.process_flow(BPE.Env.remove(:env, proc, form)), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"discard/2", z}, {:error, :"discard/2", z}, proc}
    end
  end

  def handle_call({id, :complete, stage, cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task(stage, proc), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"complete/2", z}, {:error, :"complete/2", z}, proc}
    end
  end

  def handle_call({id, :modify, form, :append, cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task([], BPE.Env.append(:env, proc, form), true), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"append/2", z}, {:error, :"append/2", z}, proc}
    end
  end

  def handle_call({id, :modify, form, :remove, cont}, _, proc) do
    try do
      handle_result(:call, id, extend_continue(process_task([], BPE.Env.remove(:env, proc, form), true), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"remove/2", z}, {:error, :"remove/2", z}, proc}
    end
  end

  def handle_call(command, _, proc) do
    handle_result(:call, :kvs.seq([], []), {:stop, :unknown, {:unknown, command}, proc}, proc)
  end

  @impl true
  def handle_cast({id, :asyncEvent, event}, proc) do
    try do
      handle_result(:cast, id, extend_continue(process_event(:async, event, proc), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"asyncEvent/2", z}, proc}
    end
  end

  def handle_cast({id, :asyncEvent, event, [continue() | _] = cont}, proc) do
    try do
      handle_result(:cast, id, extend_continue(process_event(:async, event, proc), cont, id), proc)
    catch
      _, z -> {:stop, {:error, :"asyncEvent/3", z}, proc}
    end
  end

  def handle_cast({id, :broadcastEvent, event}, proc) do
    try do
      handle_result(:cast, id, extend_continue(process_event(:async, event, proc), [], id), proc)
    catch
      _, z -> {:stop, {:error, :"broadcastEvent/2", z}, proc}
    end
  end

  def handle_cast(msg, state) do
    :logger.notice(~c"BPE: Unknown API async: ~p.", [msg])
    handle_result(:cast, :kvs.seq([], []), {:stop, {:error, {:unknown_cast, msg}}, state}, state)
  end

  @impl true
  def handle_info({:timer, :ping}, process(id: id) = state) do
    Enum.each(:kvs.all(BPE.key("/bpe/messages/queue/", id)), fn event ->
      try do
        process_event(:async, event, state)
      catch
        _, z -> z
      end
    end)
    case :application.get_env(:bpe, :ping_discipline, BPE.Ping) do
      :undefined -> {:noreply, state}
      m          -> apply(m, :ping, [state])
    end
  end

  def handle_info({:DOWN, _, _, _, _} = msg, state) do
    :logger.notice(~c"BPE: Connection closed, shutting down session: ~p.", [msg])
    handle_result(:info, :kvs.seq([], []), {:stop, :normal, state}, state)
  end

  def handle_info({:EXIT, _, reason} = msg, state) do
    :logger.notice(~c"BPE EXIT: ~p.", [msg])
    handle_result(:info, :kvs.seq([], []), {:stop, reason, state}, state)
  end

  def handle_info(info, process() = state) do
    :logger.notice(~c"BPE: Unrecognized info: ~p", [info])
    handle_result(:info, :kvs.seq([], []), {:stop, :unknown_info, state}, state)
  end

  @impl true
  def handle_continue([continue(type: :spawn, module: mod, fn: fun, args: args) | t], proc) do
    spawn(fn -> apply(mod, fun, args) end)
    {:noreply, proc, {:continue, t}}
  end

  def handle_continue([continue(id: id, type: :bpe, fn: fun, args: args) | t], proc) do
    bpe_args = convert_api_args(fun, [id | args])
    bfun     = behavior_fun(fun, bpe_args)
    bargs    = behavior_fun_args(bfun, bpe_args, [], proc)
    result   =
      try do
        apply(BPE.Proc, bfun, bargs)
      catch
        _, z -> {:stop, {:error, z}, proc}
      end
    case result do
      {:noreply, state}                     -> {:noreply, state, {:continue, t}}
      {:reply, _, state, {:continue, c}}    -> {:noreply, state, {:continue, t ++ continue_id(c, id)}}
      {:reply, _, state, _}                 -> {:noreply, state, {:continue, t}}
      {:noreply, state, {:continue, c}}     -> {:noreply, state, {:continue, t ++ continue_id(c, id)}}
      {:noreply, state, _}                  -> {:noreply, state, {:continue, t}}
      {:reply, _, state}                    -> {:noreply, state, {:continue, t}}
      {:stop, _, state}                     -> {:noreply, state, {:continue, t ++ [continue(id: id, type: :stop)]}}
      {:stop, _, _, state}                  -> {:noreply, state, {:continue, t ++ [continue(id: id, type: :stop)]}}
      x                                     -> x
    end
  end

  def handle_continue([continue(id: id, type: :stop)], proc) do
    handle_result(:continue, id, {:stop, :normal, proc}, proc)
  end

  def handle_continue([continue(type: :stop) = x | t], proc) do
    if Enum.any?(t, fn continue(type: type) -> type == :stop; _ -> false end) do
      {:noreply, proc, {:continue, t}}
    else
      {:noreply, proc, {:continue, t ++ [x]}}
    end
  end

  def handle_continue([], proc) do
    handle_result(:continue, [], {:stop, :normal, proc}, proc)
  end

  @impl true
  def terminate(reason, process(id: id) = proc) do
    BPE.cache(:terminateLocks, {:terminate, id}, self())
    :logger.notice(~c"BPE: ~ts terminate Reason: ~p ~tp", [id, reason, self()])
    Enum.each(:kvs.all(BPE.key("/bpe/messages/queue/", id)), fn event ->
      try do
        process_event(:async, event, proc)
      catch
        _, z -> z
      end
    end)
    BPE.cache(:processes, {:process, id}, :undefined)
    :ok
  end

  @impl true
  def code_change(_old_vsn, state, _extra), do: {:ok, state}
end
