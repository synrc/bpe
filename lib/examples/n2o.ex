defmodule BPE.N2O do
  require Record

  Record.defrecord(:io, code: [], data: [])
  Record.defrecord(:"Token", data: [])

  def info({:Amen, proc, docs}, r, s) do
    {:reply, {:bert, io(data: BPE.amend(:erlang.binary_to_list(proc), docs))}, r, s}
  end

  def info({:Hist, proc}, r, s) do
    {:reply, {:bert, io(data: BPE.proc_hist(:erlang.binary_to_list(proc)))}, r, s}
  end

  def info({:Proc, proc}, r, s) do
    {:reply, {:bert, io(data: BPE.proc(:erlang.binary_to_list(proc)))}, r, s}
  end

  def info({:Load, proc}, r, s) do
    {:reply, {:bert, io(data: BPE.load(:erlang.binary_to_list(proc)))}, r, s}
  end

  def info({:Next, proc}, r, s) do
    {:reply, {:bert, io(data: BPE.next(:erlang.binary_to_list(proc)))}, r, s}
  end

  def info({:Make, m, docs}, r, s) do
    {:reply, {:bert, io(data: BPE.start(apply(:io_lib.format(~c"~p",[m]), :def, []), docs))}, r, s}
  end

  def info(m, r, s), do: {:unknown, m, r, s}
end
