
-ifndef(TOUR_HRL).
-define(TOUR_HRL, true).

-record(max_tour, {count=10::integer(),joined=0::integer()}).
-record(join_application, {id=[]::integer(),name=[]::binary(),data=[]::term()}).
-record(tour_list, {users=[]::list(#join_application{})}).
-record(approve, {}).

-endif.
