-ifndef(BPE_HRL).
-define(BPE_HRL, true).

-record(process,{name,stages,rules}).
-record(stage,{name,roles,transitions,action}).

-endif.
