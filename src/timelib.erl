% Copyright (C) 2013-2015 Martin Vejmelka, UC Denver
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies
% of the Software, and to permit persons to whom the Software is furnished to do
% so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in all
% copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
% INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR
% A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
% OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
% SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

-module(timelib).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([shift_by/3,set_hour/2,round_hours/2]).
-export([hours_between/2,seconds_between/2,days_hrs_mins_secs_between/2]).
-export([seconds_elapsed_from/1]).
-export([min_time/2,max_time/2]).
-export([gregorian_seconds_now/0,local_time/0,universal_time/0]).
-export([time_range/3,list_hours/2]).
-export([to_esmf_str/1,parse_esmf/1]).


-spec shift_by(calendar:datetime(), integer(), seconds|hours|days) -> calendar:datetime().
shift_by(DT, Amt, seconds) ->
  %Shift an erlang datetime DT by <Amt> seconds positive -> future, negative -> past.
  Secs = calendar:datetime_to_gregorian_seconds(DT),
  calendar:gregorian_seconds_to_datetime(Secs + Amt);
shift_by(DT, Amt, hours) ->
  shift_by(DT, Amt * 3600, seconds);
shift_by(DT, Amt, days) ->
  shift_by(DT, Amt * 86400, seconds).


-spec set_hour(calendar:datetime(), non_neg_integer()) -> calendar:datetime().
set_hour({Date, {_H, M, S}}, H1) ->
  {Date, {H1,M,S}}.


-spec round_hours(calendar:datetime(),up|down) -> calendar:datetime().
round_hours(DT={_Date, {_H,0,0}}, _How) -> DT;
round_hours({Date, {H,_M,_S}}, down) -> {Date, {H,0,0}};
round_hours({Date, {H,_M,_S}}, up) -> shift_by({Date, {H, 0, 0}}, 1, hours).


-spec seconds_between(calendar:datetime(), calendar:datetime()) -> integer().
seconds_between(From, To) ->
    FS = calendar:datetime_to_gregorian_seconds(From),
    TS = calendar:datetime_to_gregorian_seconds(To),
    TS - FS.


% Find the number of days/hours/minutes/seconds between <from> and <to> and
% return it as a list."
-spec days_hrs_mins_secs_between(calendar:datetime(),calendar:datetime()) -> [integer()].
days_hrs_mins_secs_between(From, To) ->
    S = seconds_between(From, To),
    element(1, lists:mapfoldl( fun(D, R) -> {R div D, R rem D} end, S, [86400,3600,60,1])).


% Compute the number of whole hours between <from> and <to> assuming
% that to > from."
-spec hours_between(calendar:datetime(),calendar:datetime()) -> number().
hours_between(From,To) -> seconds_between(From, To) / 3600.


% Compute seconds elapsed from <From>.
seconds_elapsed_from(From) -> seconds_between(From, calendar:local_time()).


% Return the earlier time of the two times.
-spec min_time(calendar:datetime(),calendar:datetime()) -> calendar:datetime().
min_time(T1, T2) when T1 < T2 -> T1;
min_time(_T1, T2) -> T2.


% Return the later of the two times.
-spec max_time(calendar:datetime(),calendar:datetime()) -> calendar:datetime().
max_time(T1, T2) when T1 > T2 -> T1;
max_time(_T1, T2) -> T2.


-spec gregorian_seconds_now() -> non_neg_integer().
gregorian_seconds_now() -> calendar:datetime_to_gregorian_seconds(calendar:local_time()).

-spec local_time() -> calendar:datetime().
local_time() -> calendar:local_time().

-spec universal_time() -> calendar:datetime().
universal_time() -> calendar:universal_time().

-spec time_range(calendar:datetime(),calendar:datetime(),integer()) -> [calendar:datetime()].
time_range(From, To, _SecIncr) when From > To -> [];
time_range(From, To, SecIncr) -> [From|time_range(shift_by(From, SecIncr, seconds), To, SecIncr)].

-spec list_hours(calendar:datetime(),calendar:datetime()) -> [calendar:datetime()].
list_hours(From, To) -> time_range(From, To, 3600).

-spec to_esmf_str(calendar:datetime()) -> string().
to_esmf_str({{Y,M,D},{H,Mi,S}}) ->
  io_lib:format("~4..0B-~2..0B-~2..0B_~2..0B:~2..0B:~2..0B", [Y,M,D,H,Mi,S]).

% Parse an ESMF date/time string (format: YYYY-MM-DD_HH:mm:SS) <str> into an erlang datetime.
% The string must have exactly 19 characters."
-spec parse_esmf(string()) -> calendar:datetime().
parse_esmf([Y1,Y2,Y3,Y4,$-,M1,M2,$-,D1,D2,$_,H1,H2,$:,Mi1,Mi2,$:,S1,S2]) ->
  {{list_to_integer([Y1,Y2,Y3,Y4]), list_to_integer([M1,M2]), list_to_integer([D1,D2])},
   {list_to_integer([H1,H2]), list_to_integer([Mi1,Mi2]), list_to_integer([S1,S2])}}.
    

