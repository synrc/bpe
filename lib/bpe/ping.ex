defmodule BPE.Ping do
  require BPE
  import BPE

  defp timer_restart({x, y, z}) do
    :erlang.send_after(500 * (z + 60 * y + 60 * 60 * x), self(), {:timer, :ping})
  end

  def ping,        do: :application.get_env(:bpe, :ping, {0, 0, 30})
  def termination, do: :application.get_env(:bpe, :boundary, {7, {0, 0, 0}})

  def ping(process(timer: timer, id: id, modified: ts(time: time2)) = state) do
    unless timer == [], do: :erlang.cancel_timer(timer)
    case BPE.head(id) do
      hist(time: ts(time: time1)) ->
        {d1, diff1} = :calendar.time_difference(time1, :calendar.local_time())
        {d2, diff2} = :calendar.time_difference(time2, :calendar.local_time())
        if {d1, diff1} > termination() or {d2, diff2} > termination() do
          {:stop, :normal, state}
        else
          {:noreply, process(state, timer: timer_restart(ping()))}
        end
      _ -> :skip
    end
  end
end
