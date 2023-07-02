%%%-------------------------------------------------------------------
%%% @author Lionel Pinkhard
%%% @doc
%%% Handles date manipulation
%%% @end
%%% Created : 07. Oct 2020 22:39
%%%-------------------------------------------------------------------
-module(dates).
-author("Lionel Pinkhard").

%% API
-export([convert_date/1, date_difference/2, current_date/0, current_date_flip/0, 
  days_before/1, days_before_flip/1]).

date_difference(Date1, Date2) ->
  Date1I = date_to_int(Date1),
  Date2I = date_to_int(Date2),
  abs(Date1I - Date2I).

date_to_int(Date) ->
  <<Year:4/binary, _:1/binary, Month:2/binary, _:1/binary, Day:2/binary>> = list_to_binary(Date),
  binary_to_integer(Year) * 372 + binary_to_integer(Month) * 31 + binary_to_integer(Day).

month_convert(MonthName) ->
  case MonthName of
    "Jan" -> "01";
    "Feb" -> "02";
    "Mar" -> "03";
    "Apr" -> "04";
    "May" -> "05";
    "Jun" -> "06";
    "Jul" -> "07";
    "Aug" -> "08";
    "Sep" -> "09";
    "Oct" -> "10";
    "Nov" -> "11";
    "Dec" -> "12"
  end.

get_month_name(Month) ->
  case Month of
    1 -> "Jan";
    2 -> "Feb";
    3 -> "Mar";
    4 -> "Apr";
    5 -> "May";
    6 -> "Jun";
    7 -> "Jul";
    8 -> "Aug";
    9 -> "Sep";
    10 -> "Oct";
    11 -> "Nov";
    12 -> "Dec"
  end.

current_date() ->
  {{Year, Month, Day}, {_, _, _}} = erlang:localtime(),
  io_lib:format("~4..0w~s~2..0w~s~2..0w", [Year, "-", Month, "-", Day]).

current_date_flip() ->
  {{Year, Month, Day}, {_, _, _}} = erlang:localtime(),
  io_lib:format("~s~s~2..0w~s~4..0w", [get_month_name(Month), "-", Day, "-", Year]).

days_before(Offset) ->
  Sub = Offset * 86400,
  {{Year, Month, Day}, {_, _, _}} = 
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) - Sub),
  io_lib:format("~4..0w~s~2..0w~s~2..0w", [Year, "-", Month, "-", Day]).

days_before_flip(Offset) ->
  Sub = Offset * 86400,
  {{Year, Month, Day}, {_, _, _}} =
    calendar:gregorian_seconds_to_datetime(calendar:datetime_to_gregorian_seconds(erlang:localtime()) - Sub),
  io_lib:format("~s~s~2..0w~s~4..0w", [get_month_name(Month), "-", Day, "-", Year]).

convert_date(Date) ->
  <<_:5/binary, Day:2/binary, MonthName:4/binary, Year:5/binary, _/binary>> = list_to_binary(Date),
  string:strip(binary_to_list(Year)) ++ "-" ++ month_convert(string:strip(binary_to_list(MonthName)))
    ++ "-" ++ io_lib:format("~2..0B", [list_to_integer(string:strip(binary_to_list(Day)))]).
