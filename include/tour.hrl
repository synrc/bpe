
-ifndef(TOUR_HRL).
-define(TOUR_HRL, true).

-record(max_tour, {count=10,joined=0}).
-record(tour_list, {users=[]}).
-record(join_application, {id=[],name=[],data=[]}).

-endif.
