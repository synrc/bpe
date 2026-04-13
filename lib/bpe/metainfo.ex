defmodule BPE.Metainfo do
  require BPE
  import BPE

  def metainfo do
    schema(
      name: :bpe,
      tables: [
        table(name: :process,        fields: fields(:process),        instance: process()),
        table(name: :monitor,        fields: fields(:monitor),        instance: monitor()),
        table(name: :procRec,        fields: fields(:procRec),        instance: procRec()),
        table(name: :hist,           fields: fields(:hist),           instance: hist()),
        table(name: :sched,          fields: fields(:sched),          instance: sched()),
        table(name: :broadcastEvent, fields: fields(:broadcastEvent), instance: broadcastEvent()),
        table(name: :messageEvent,   fields: fields(:messageEvent),   instance: messageEvent()),
        table(name: :asyncEvent,     fields: fields(:asyncEvent),     instance: asyncEvent()),
        table(name: :subscription,   fields: fields(:subscription),   instance: subscription(),
              keys: fields(:subscription)),
        table(name: :gw_block,       fields: fields(:gw_block),       instance: gw_block(),
              keys: fields(:gw_block)),
        table(name: :terminateLock,  fields: fields(:terminateLock),  instance: terminateLock(),
              keys: fields(:terminateLock))
      ]
    )
  end

  defp fields(name) do
    Record.extract(name, from_lib: "bpe/include/bpe.hrl") |> Keyword.keys()
  end
end
