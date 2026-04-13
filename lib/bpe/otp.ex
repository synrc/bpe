defmodule BPE.OTP do
  require BPE
  import BPE

  @behaviour :application
  @behaviour :supervisor

  def stop(_), do: :ok

  def respawn do
    spawn(fn -> Enum.each(:kvs.all("/bpe/proc"), &worker/1) end)
  end

  def start(_, _) do
    Enum.each(
      :application.get_env(:bpe, :procmodules, [BPE.Account, BPE]),
      &BPE.reload/1
    )
    :logger.add_handlers(:bpe)
    :kvs.join()
    :kvs.join([], kvs(mod: :kvs_mnesia))
    :kvs.ensure(:kvs.writer("/bpe/proc"))
    :supervisor.start_link({:local, __MODULE__}, __MODULE__, [])
  end

  def init([]) do
    opt = [:set, :named_table, {:keypos, 1}, :public]
    Enum.each([:processes, :terminateLocks], fn t -> :ets.new(t, opt) end)
    {:ok, {{:one_for_one, 5, 10}, []}}
  end

  defp worker(process(id: id) = p) do
    case BPE.head(id) do
      hist(time: ts(time: time)) ->
        worker_do(:calendar.time_difference(time, :calendar.local_time()), p)
      _ -> :broken
    end
  end

  defp worker(p), do: :logger.notice(~c"BPE: Unknown ~p", [p])

  defp worker_do({days, _}, _) when days >= 14, do: :skip
  defp worker_do({_, _}, process(status: "deleted")), do: :skip
  defp worker_do({_, _}, process(docs: docs) = p), do: BPE.start(p, docs)
end
