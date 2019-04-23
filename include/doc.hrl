-ifndef(BPE_DOCS_HRL).
-define(BPE_DOCS_HRL, true).

-record(max_tour, {count=10::integer(),joined=0::integer()}).
-record(join_application, {id=[]::[]|integer(),name=[]::[]|atom()|term(),data=[]::[]|integer()|term()}).
-record(tour_list, {users=[]::list(#join_application{})}).
-record(approve, {}).
-record(close_account, {}).
-record(tx, {}).

-endif.
