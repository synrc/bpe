-module(bpe_date).
-author('David Weldon').
-compile(export_all).

beginning_of_month({Y, M, _D}) -> {Y, M, 1}.
date_to_string({Y, M, D}) ->
    true = calendar:valid_date({Y, M, D}),
    lists:flatten(io_lib:format("~4..0B-~2..0B-~2..0B", [Y, M, D])).

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

end_of_month({Y, M, _D}) -> {Y, M, calendar:last_day_of_the_month(Y, M)}.
is_after(Date1, Date2) -> subtract(Date1, Date2) > 0.
is_before(Date1, Date2) -> subtract(Date1, Date2) < 0.
is_in_future(Date) -> subtract(Date, today()) > 0.
is_in_past(Date) -> subtract(Date, today()) < 0.
shift(N, Period) -> shift(date(), N, Period).
shift(Date, N, Period) when Period =:= day; Period =:= days -> calendar:gregorian_days_to_date(calendar:date_to_gregorian_days(Date) + N);
shift(Date, N, Period) when Period =:= week; Period =:= weeks -> shift(Date, 7*N, days);
shift(Date, N, Period) when Period =:= year; Period =:= years -> shift(Date, 12*N, months);
shift({Y, M, D}, N, Period) when Period =:= month; Period =:= months ->
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

find_valid_date(Date) ->
    case calendar:valid_date(Date) of
        true -> Date;
        false ->
            {Y, M, D} = Date,
            find_valid_date({Y, M, D-1})
    end.

string_to_date(String) ->
    [Year, Month, Day] =
        case string:tokens(String, "-/") of
            [Y, M, D] when length(Y) =:= 4 -> [Y, M, D];
            [M, D, Y] when length(Y) =:= 4 -> [Y, M, D]
        end,
    Date = list_to_tuple([list_to_integer(X) || X <- [Year, Month, Day]]),
    true = calendar:valid_date(Date),
    Date.

subtract(Date1, Date2) ->
    calendar:date_to_gregorian_days(Date1) -
    calendar:date_to_gregorian_days(Date2).

today() -> date().
tomorrow() -> shift(1, day).
yesterday() -> shift(-1, day).

