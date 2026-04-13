defmodule BPE.Env do
  require BPE
  import BPE

  def append(:kvs, feed, rec), do: :kvs.append(rec, feed)
  def append(:env, p, []), do: p
  def append(:env, p, [rec | tail]), do: append(:env, append(:env, p, rec), tail)

  def append(:env, process() = p, rec) when is_tuple(rec) do
    feed = "/bpe/proc"
    s =
      case find(:env, p, rec) do
        {[], rest}     -> process(p, docs: [rec | rest])
        {found, rest}  -> process(p, docs: [rec | found] ++ rest)
      end
    :kvs.append(s, feed)
    x = process(s, modified: ts(time: :calendar.local_time()))
    :kvs.put(x)
    x
  end

  def find(rec, feed) do
    pairs =
      for {pos, val} <- Enum.zip(1..tuple_size(rec), Tuple.to_list(rec)), val != [], do: {pos, val}
    Enum.split_with(feed, fn r ->
      Enum.all?(pairs, fn {p, x} -> elem(r, p - 1) == x end)
    end)
  end

  def find(:kvs, feed, rec), do: find(rec, :kvs.all(feed))
  def find(:env, proc, rec), do: find(rec, process(proc, :docs))

  def remove(:env, p, []), do: p
  def remove(:env, p, [rec | tail]), do: remove(:env, remove(:env, p, rec), tail)

  def remove(:kvs, feed, rec) when is_tuple(rec) do
    {x, _} = find(:kvs, feed, rec)
    Enum.each(x, fn i -> :kvs.delete(feed, elem(i, 1)) end)
  end

  def remove(:env, proc, rec) when is_tuple(rec) do
    {_, y} = find(:env, proc, rec)
    s = process(proc, docs: y, modified: ts(time: :calendar.local_time()))
    :kvs.append(s, "/bpe/proc")
    :kvs.put(s)
    s
  end
end
