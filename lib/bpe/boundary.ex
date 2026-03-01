defmodule BPE.Boundary do
  require BPE
  import BPE

  defp timer_restart({x, y, z}) do
    :erlang.send_after(500 * (z + 60 * y + 60 * 60 * x), self(), {:timer, :ping})
  end

  def ping, do: :application.get_env(:bpe, :ping, {0, 0, 5})

  def ping(process(timer: timer, id: id, events: events, notifications: pid) = state) do
    {_, task} = BPE.current_task(state)
    unless timer == [], do: :erlang.cancel_timer(timer)

    terminal =
      case :lists.keytake(:*, messageEvent(:id) + 1, events) do
        {:value, event, _} ->
          {:*, elem(event, 0), :erlang.element(messageEvent(:timeout) + 1, event)}
        false ->
          {:*, :none, timeout(spec: {:none, :none})}
      end

    {name, record, timeout(spec: {days, pattern})} =
      case :lists.keytake(task, messageEvent(:name) + 1, events) do
        {:value, event2, _} ->
          {task, elem(event2, 0), :erlang.element(messageEvent(:timeout) + 1, event2)}
        false -> terminal
      end

    {dd, diff} =
      case BPE.head(id) do
        hist(time: ts(time: time1)) -> :calendar.time_difference(time1, :calendar.local_time())
        _ -> {:immediate, :timeout}
      end

    case {{dd, diff} < {days, pattern}, record} do
      {_, :none} ->
        {:noreply, process(state, timer: timer_restart(ping()))}
      {true, _} ->
        {:noreply, process(state, timer: timer_restart(ping()))}
      {false, :timeoutEvent} ->
        :logger.notice(~c"BPE: ~p complete Timeout: ~p", [id, {dd, diff}])
        case BPE.Proc.process_task([], state) do
          {:reply, _, new_state} -> {:noreply, process(new_state, timer: timer_restart(ping()))}
          {:stop, :normal, _, new_state} -> {:stop, :normal, new_state}
        end
      {false, _record} ->
        :logger.notice(~c"BPE: ~p close ~p Timeout: ~p", [id, record, {dd, diff}])
        if is_pid(pid), do: Kernel.send(pid, {:direct, {:bpe, :terminate, {name, {days, pattern}}}})
        BPE.cache(:processes, {:process, id}, :undefined)
        {:stop, :normal, state}
    end
  end
end
