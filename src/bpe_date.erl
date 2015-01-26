%% @author David Weldon
%% @copyright 2010 David Weldon
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @doc edate is a date manipulation library.

-module(bpe_date).
-export([beginning_of_month/1,
         date_to_string/1,
         day_of_week/1,
         easter/1,
         end_of_month/1,
         is_after/2,
         is_before/2,
         is_in_future/1,
         is_in_past/1,
         shift/2,
         shift/3,
         string_to_date/1,
         subtract/2,
         today/0,
         tomorrow/0,
         yesterday/0]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type year()    :: non_neg_integer().
-type month()   :: 1..12.
-type day()     :: 1..31.
-type date()    :: {year(), month(), day()}.
-type period()  :: day | days | week | weeks | month | months | year | years.

%% @type year() = integer().
%% @type month() = integer(). 1..12
%% @type day() = integer(). 1..31
%% @type date() = {year(), month(), day()}.
%% @type period() = day | days | week | weeks | month | months | year | years.

%% @spec beginning_of_month(Date::date()) -> date()
%% @doc Returns the first date of the month containing `Date'.
%% ```
%% > edate:beginning_of_month({2010,2,15}).
%% {2010,2,1}
%% '''
-spec beginning_of_month(date()) -> date().
beginning_of_month({Y, M, _D}) -> {Y, M, 1}.

%% @spec date_to_string(Date::date()) -> string()
%% @doc Returns an <a href="http://en.wikipedia.org/wiki/Iso8601">ISO 8601</a>
%% representation of Date.
%% ```
%% > edate:date_to_string({1976,3,18}).
%% "1976-03-18"
%% '''
-spec date_to_string(date()) -> string().
date_to_string({Y, M, D}) ->
    true = calendar:valid_date({Y, M, D}),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

%% @spec day_of_week(date()) -> string()
%% @doc Returns the day of the week as a string ("monday".."sunday") for `Date'.
%% ```
%% > edate:day_of_week({2010,7,4}).
%% "sunday"
%% '''
-spec day_of_week(date()) -> string().
day_of_week(Date) ->
    case calendar:day_of_the_week(Date) of
        1 -> "monday";
        2 -> "tuesday";
        3 -> "wednesday";
        4 -> "thursday";
        5 -> "friday";
        6 -> "saturday";
        7 -> "sunday"
    end.

%% @spec easter(year()) -> date()
%% @doc Returns the date for Easter (Roman Catholic) in `Year'. Derived from
%% [http://www.gmarts.org/index.php?go=415#geteasterdatec].
%% ```
%% > edate:easter(2010).
%% {2010,4,4}
%% '''
-spec easter(year()) -> date().
easter(Year) ->
    NCent = Year div 100,
    NRemain19 = Year rem 19,
    N1Tmp1 = fix(NCent, ((NCent - 15) div 2) + 202 - (11 * NRemain19)),
    N1Tmp2 = N1Tmp1 rem 30,
    N1 =
        case N1Tmp2 == 29 orelse (N1Tmp2 == 28 andalso NRemain19 > 10) of
            true -> N1Tmp2 - 1;
            false -> N1Tmp2
        end,
    DtPFM =
        case N1 > 10 of
            true -> {Year, 4, N1 - 10};
            false -> {Year, 3, N1 + 21}
        end,
    NWeekDay = calendar:day_of_the_week(DtPFM) rem 7,
    shift(DtPFM, 7 - NWeekDay, days).

fix(NCent, N1) when NCent >= 38 -> N1 - 2;
fix(NCent, N1) when NCent == 21 orelse NCent == 24 orelse NCent == 25 -> N1 - 1;
fix(NCent, N1) when NCent == 33 orelse NCent == 36 orelse NCent == 37 -> N1 - 2;
fix(NCent, N1) when NCent > 26 -> N1 - 1;
fix(_NCent, N1) -> N1.

%% @spec end_of_month(Date::date()) -> date()
%% @doc Returns the last date of the month containing `Date'.
%% ```
%% > edate:end_of_month({2010,2,15}).
%% {2010,2,28}
%% '''
-spec end_of_month(date()) -> date().
end_of_month({Y, M, _D}) -> {Y, M, calendar:last_day_of_the_month(Y, M)}.

%% @spec is_after(date(), date()) -> boolean()
%% @doc Returns `true' if `Date1' is after `Date2', and `false' otherwise.
%% ```
%% > edate:is_after({1950,7,2}, {1950,7,1}).
%% true
%% '''
-spec is_after(date(), date()) -> boolean().
is_after(Date1, Date2) -> subtract(Date1, Date2) > 0.

%% @spec is_before(date(), date()) -> boolean()
%% @doc Returns `true' if `Date1' is before `Date2', and `false' otherwise.
%% ```
%% > edate:is_before({1950,7,1}, {1950,7,2}).
%% true
%% '''
-spec is_before(date(), date()) -> boolean().
is_before(Date1, Date2) -> subtract(Date1, Date2) < 0.

%% @spec is_in_future(date()) -> boolean()
%% @doc Returns `true' if `Date' is in the future, and `false' otherwise.
%% ```
%% > edate:is_in_future({3000,7,1}).
%% true
%% '''
-spec is_in_future(date()) -> boolean().
is_in_future(Date) -> subtract(Date, today()) > 0.

%% @spec is_in_past(date()) -> boolean()
%% @doc Returns `true' if `Date' is in the past, and `false' otherwise.
%% ```
%% > edate:is_in_past({1950,7,1}).
%% true
%% '''
-spec is_in_past(date()) -> boolean().
is_in_past(Date) -> subtract(Date, today()) < 0.

%% @spec shift(integer(), period()) -> date()
%% @doc Returns a new date after shifting today's date by `N' periods.
-spec shift(integer(), period()) -> date().
shift(N, Period) -> shift(date(), N, Period).

%% @spec shift(date(), integer(), period()) -> date()
%% @doc Returns a new date after shifting `Date' by `N' periods.
%% ```
%% > edate:shift({2000,1,1}, -2, days).
%% {1999,12,30}
%% > edate:shift({2010,2,27}, 1, week).
%% {2010,3,6}
%% '''
-spec shift(date(), integer(), period()) -> date().
shift(Date, N, Period) when Period =:= day; Period =:= days ->
    calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + N);
shift(Date, N, Period) when Period =:= week; Period =:= weeks ->
    shift(Date, 7*N, days);
shift(Date, N, Period) when Period =:= year; Period =:= years ->
    shift(Date, 12*N, months);
shift({Y, M, D}, N, Period) when Period =:= month; Period =:= months ->
    %% in order for the modular arithmetic to work, months in this function range
    %% from 0 to 11 (January to December)
    TotalMonths = 12*Y + M-1 + N,
    case TotalMonths >= 0 of
        true ->
            Month = TotalMonths rem 12,
            Year = (TotalMonths - Month) div 12,
            %% add one back to the month to fix our mod 12 shenanigans
            find_valid_date({Year, Month+1, D});
        false ->
            error(out_of_bounds)
    end.

%% @spec find_valid_date(date()) -> date()
%% @doc Returns `Date' if valid. Otherwise, backward searches for a valid date.
-spec find_valid_date(date()) -> date().
find_valid_date(Date) ->
    case calendar:valid_date(Date) of
        true -> Date;
        false ->
            {Y, M, D} = Date,
            find_valid_date({Y, M, D-1})
    end.

%% @spec string_to_date(string()) -> date()
%% @doc Converts `String' to a date. The following string formats are valid
%% (with or without zero-padding):
%% YYYY-MM-DD, YYYY/MM/DD, MM-DD-YYYY, MM/DD/YYYY.
%% ```
%% > edate:string_to_date("1976-03-18").
%% {1976,3,18}
%% > edate:string_to_date("3/18/1976").
%% {1976,3,18}
%% '''
-spec string_to_date(string()) -> date().
string_to_date(String) ->
    [Year, Month, Day] =
        case string:tokens(String, "-/") of
            [Y, M, D] when length(Y) =:= 4 -> [Y, M, D];
            [M, D, Y] when length(Y) =:= 4 -> [Y, M, D]
        end,
    Date = list_to_tuple([list_to_integer(X) || X <- [Year, Month, Day]]),
    true = calendar:valid_date(Date),
    Date.

%% @spec subtract(date(), date()) -> integer()
%% @doc Returns `Date1' - `Date2' as an integer number of days. The number of
%% days returned will be positive if `Date1' > `Date2'.
%% ```
%% > edate:subtract({2010,7,4}, {2010,7,1}).
%% 3
%% > edate:subtract({2010,7,1}, {2010,7,4}).
%% -3
%% '''
-spec subtract(date(), date()) -> integer().
subtract(Date1, Date2) ->
    calendar:date_to_gregorian_days(Date1) -
    calendar:date_to_gregorian_days(Date2).

%% @spec today() -> date()
%% @doc Returns today's date.
-spec today() -> date().
today() -> date().

%% @spec tomorrow() -> date()
%% @doc Returns tomorrow's date.
-spec tomorrow() -> date().
tomorrow() -> shift(1, day).

%% @spec yesterday() -> date()
%% @doc Returns yesterday's date.
-spec yesterday() -> date().
yesterday() -> shift(-1, day).

-ifdef(TEST).

beginning_of_month_test_() ->
    [?_assertEqual({2012,2,1}, beginning_of_month({2012,2,15})),
     ?_assertEqual({2012,2,1}, beginning_of_month({2012,2,1}))].

date_to_string_test_() ->
    [?_assertEqual("1976-12-20", date_to_string({1976,12,20})),
     ?_assertEqual("1976-03-18", date_to_string({1976,3,18})),
     ?_assertEqual("2010-01-02", date_to_string({2010,1,2}))].

day_of_week_test_() ->
    [?_assertEqual("monday", day_of_week({2010,6,28})),
     ?_assertEqual("tuesday", day_of_week({2010,6,29})),
     ?_assertEqual("wednesday", day_of_week({2010,6,30})),
     ?_assertEqual("thursday", day_of_week({2010,7,1})),
     ?_assertEqual("friday", day_of_week({2010,7,2})),
     ?_assertEqual("saturday", day_of_week({2010,7,3})),
     ?_assertEqual("sunday", day_of_week({2010,7,4}))].

easter_test_() ->
    [?_assertEqual({2008,3,23}, easter(2008)),
     ?_assertEqual({2009,4,12}, easter(2009)),
     ?_assertEqual({2010,4,4}, easter(2010)),
     ?_assertEqual({2011,4,24}, easter(2011)),
     ?_assertEqual({2012,4,8}, easter(2012)),
     ?_assertEqual({2013,3,31}, easter(2013)),
     ?_assertEqual({2014,4,20}, easter(2014))].

end_of_month_test_() ->
    [?_assertEqual({2012,2,29}, end_of_month({2012,2,29})),
     ?_assertEqual({2012,2,29}, end_of_month({2012,2,1})),
     ?_assertEqual({2010,2,28}, end_of_month({2010,2,28})),
     ?_assertEqual({2010,2,28}, end_of_month({2010,2,1}))].

is_after_test_() ->
    [?_assertEqual(true, is_after({2011,7,14}, {2011,7,13})),
     ?_assertEqual(false, is_after({2011,7,14}, {2011,7,14})),
     ?_assertEqual(false, is_after({2011,7,13}, {2011,7,14}))].

is_before_test_() ->
    [?_assertEqual(false, is_before({2011,7,14}, {2011,7,13})),
     ?_assertEqual(false, is_before({2011,7,14}, {2011,7,14})),
     ?_assertEqual(true, is_before({2011,7,13}, {2011,7,14}))].

is_in_future_test_() ->
    [?_assertEqual(true, is_in_future(tomorrow())),
     ?_assertEqual(false, is_in_future(yesterday())),
     ?_assertEqual(false, is_in_future(today()))].

is_in_past_test_() ->
    [?_assertEqual(false, is_in_past(tomorrow())),
     ?_assertEqual(true, is_in_past(yesterday())),
     ?_assertEqual(false, is_in_past(today()))].

shift_test_() ->
    [?_assertEqual(date(), shift(0, days)),
     ?_assertEqual(date(), shift(0, days)),
     ?_assertEqual(date(), shift(0, months)),
     ?_assertEqual(date(), shift(0, years)),
     %% relative dates
     ?_assertEqual(tomorrow(), shift(1, day)),
     ?_assertEqual(yesterday(), shift(-1, day)),
     %% simple day addition
     ?_assertEqual({2000,1,2}, shift({2000,1,1}, 1, day)),
     ?_assertEqual({2000,1,3}, shift({2000,1,1}, 2, days)),
     %% simple week addition
     ?_assertEqual({2000,1,8}, shift({2000,1,1}, 1, week)),
     ?_assertEqual({2000,1,15}, shift({2000,1,1}, 2, weeks)),
     %% simple month addition
     ?_assertEqual({2000,2,1}, shift({2000,1,1}, 1, month)),
     ?_assertEqual({2000,3,1}, shift({2000,1,1}, 2, months)),
     %% simple year addition
     ?_assertEqual({2003,1,1}, shift({2000,1,1}, 3, years)),
     %% simple year subtraction
     ?_assertEqual({1997,1,1}, shift({2000,1,1}, -3, years)),
     %% day subtraction at year boundary
     ?_assertEqual({1999,12,31}, shift({2000,1,1}, -1, day)),
     ?_assertEqual({1999,12,30}, shift({2000,1,1}, -2, days)),
     %% week subtraction at year boundary
     ?_assertEqual({1999,12,25}, shift({2000,1,1}, -1, week)),
     ?_assertEqual({1999,12,18}, shift({2000,1,1}, -2, weeks)),
     %% month subtraction at year boundary
     ?_assertEqual({1999,12,1}, shift({2000,1,1}, -1, month)),
     ?_assertEqual({1999,11,1}, shift({2000,1,1}, -2, months)),
     %% month subtraction before year 0
     ?_assertError(out_of_bounds, shift({0, 1, 1}, -1, month)),
     %% 1 year = 12 months = 365 days (in a non-leap year)
     ?_assertEqual(shift({2001,5,10}, 1, year), shift({2001,5,10}, 12, months)),
     ?_assertEqual(shift({2001,5,10}, 1, year), shift({2001,5,10}, 365, days)),
     %% date rounding from month addition and subtraction
     ?_assertEqual({2001,2,28}, shift({2001,1,31}, 1, month)),
     ?_assertEqual({2001,2,28}, shift({2001,3,31}, -1, month)),
     %% leap year
     ?_assertEqual({2012,2,29}, shift({2012,1,31}, 1, month)),
     ?_assertEqual({2012,2,29}, shift({2012,4,30}, -2, months)),
     ?_assertEqual({2013,2,28}, shift({2012,2,29}, 1, year))].

string_to_date_test_() ->
    [?_assertEqual({2010,1,2}, string_to_date("2010-01-02")),
     ?_assertEqual({2010,1,2}, string_to_date("2010-1-2")),
     ?_assertEqual({2010,1,2}, string_to_date("2010/01/02")),
     ?_assertEqual({2010,1,2}, string_to_date("2010/1/2")),
     ?_assertEqual({2010,1,2}, string_to_date("01-02-2010")),
     ?_assertEqual({2010,1,2}, string_to_date("1-2-2010")),
     ?_assertEqual({2010,1,2}, string_to_date("01/02/2010")),
     ?_assertEqual({2010,1,2}, string_to_date("1/2/2010"))].

subtract_test_() ->
    [?_assertEqual(3, subtract({2010,7,4}, {2010,7,1})),
     ?_assertEqual(-3, subtract({2010,7,1}, {2010,7,4})),
     ?_assertEqual(1, subtract(today(), yesterday())),
     ?_assertEqual(0, subtract(today(), today()))].

-endif.