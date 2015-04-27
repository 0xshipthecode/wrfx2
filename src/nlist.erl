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

-module(nlist).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([parse/2,render_nlist/1,render_namelists/1,set_entries/2,get_entry/3,set_entry/4]).
-export([get_values_for_dom/4,set_values_for_dom/4,set_values_for_doms/4]).
-export([set_wrf_start_time/3,set_wrf_end_time/3,set_wps_start_time/3,set_wps_end_time/3]).
-export([set_wrf_run_time/3]).
-export([max_domains/1,all_domains/1]).
-export([set_ignition/4,set_ignitions/3]).

-include("wrfx2.hrl").

-spec parse(string(),string()) -> #nls{}.
parse(Path,Name) ->
  case nlparser:parse(nlscanner:scan(Path)) of
    {ok, Ns} -> #nls{name=Name,nlists=Ns};
    {error, {Line,_Mod,Text}} -> 
      Msg = lists:flatten(io_lib:format("~p on line ~p", [Text,Line])),
      throw({error, Msg})
  end.


-spec render_value(term()) -> string().
render_value(false) -> ".false.";
render_value(true)  -> ".true.";
render_value(L) when is_list(L) -> io_lib:format("'~s'", [L]);
render_value(O) -> io_lib:format("~p", [O]).


-spec render_entry({string(),list()}) -> string().
render_entry({Key,Vals}) ->
  [Key, lists:duplicate(36 - length(Key), $ ), "=\t",
   string:join(lists:map(fun render_value/1, Vals), ",\t"), " \n"].


-spec render_nlist({string(),list()}) -> string().
render_nlist({Name,Es}) ->
  ["&",Name,"\n",lists:map(fun render_entry/1,Es), "/\n\n"].


-spec render_namelists(#nls{}) -> list().
render_namelists(#nls{nlists=Ns}) -> lists:flatten(lists:map(fun render_nlist/1, Ns)).
 
-spec has_nl(string(),#nls{}) -> boolean().
has_nl(Name,#nls{nlists=Ns}) -> plist:contains(Name,Ns).

-spec get_nl(string(),#nls{}) -> plist().
get_nl(Name,#nls{nlists=Ns}) -> plist:get(Name,Ns).

-spec set_nl(string(),plist(),#nls{}) -> #nls{}.
set_nl(Name,Nl,#nls{name=N,nlists=Ns0}) -> #nls{name=N,nlists=plist:set(Name,Nl,Ns0)}.

-spec get_entry(string(),string(),#nls{}) -> list().
get_entry(Name,Key,Nls) -> plist:get(Key,get_nl(Name,Nls)).

-spec set_entry(string(),string(),list(),#nls{}) -> #nls{}.
set_entry(Name,Key,Vals,Nls) -> set_nl(Name,plist:set(Key,Vals,get_nl(Name,Nls)),Nls).

-spec update_entries({string(),string()},#nls{}) -> #nls{}.
update_entries({Name,Kvs},Nls) -> set_nl(Name,plist:update_with(Kvs,get_nl(Name,Nls)),Nls).

-spec set_entries(plist(),#nls{}) -> #nls{}.
set_entries(Ess,Nls) -> lists:foldl(fun(Es,Ns0) -> update_entries(Es,Ns0) end, Nls, Ess).


-spec update_list([integer()],term(),list()) -> list().
update_list(Ndxs,Val,Lst) -> update_list(Lst,Val,Ndxs,1,[]).
update_list(Lst,_Val,[],_Curr,Acc) -> lists:reverse(Acc,Lst);
update_list([],Val,[Curr|Rst],Curr,Acc) -> update_list([],Val,Rst,Curr+1,[Val|Acc]);
update_list([_|Tl],Val,[Curr|Rst],Curr,Acc) -> update_list(Tl,Val,Rst,Curr+1,[Val|Acc]);
update_list([],_Val,[_N|_Rst],_Curr,_Acc) -> throw(update_list_bad_index);
update_list([Hd|Tl],Val,Ndx,Curr,Acc) -> update_list(Tl,Val,Ndx,Curr+1,[Hd|Acc]).


-spec set_nth(integer(),term(),list()) -> list().
set_nth(Ndx,Val,Lst) -> set_nth(Ndx,Val,Lst,[]).
set_nth(1,Val,[],Acc) -> lists:reverse([Val|Acc]);
set_nth(_,_,[],_) -> throw(set_nth_past_end_of_list);
set_nth(1,Val,[_|Tl],Acc) -> lists:reverse([Val|Acc],Tl);
set_nth(N,Val,[Hd|Tl],Acc) when N > 1 -> set_nth(N-1,Val,Tl,[Hd|Acc]).


-spec get_values_for_dom(#nls{},string(),list(),integer()) -> #nls{}.
get_values_for_dom(Nls,Name,Ks,Dom) -> 
  Vals = plist:get_list(Ks,get_nl(Name,Nls)),
  lists:map(fun (X) -> lists:nth(Dom,X) end, Vals).


-spec set_values_for_dom(#nls{},string(),plist(),integer()) -> #nls{}.
set_values_for_dom(Nls,Name,Kvs,Dom) ->
  Nl0 = get_nl(Name,Nls),
  Upd = fun(Key,Val,Ndx,Ps) -> plist:set(Key,set_nth(Ndx,Val,plist:get(Key,Ps)),Ps) end,
  Nl1 = lists:foldl(fun({Key,Val},A) -> Upd(Key,Val,Dom,A) end, Nl0, Kvs),
  set_nl(Name,Nl1,Nls).


-spec set_values_for_doms(#nls{},string(),plist(),[integer()]) -> #nls{}.
set_values_for_doms(Nls,Name,Kvs,Doms) ->
  Nl0 = get_nl(Name,Nls),
  Upd = fun(Key,Val,Ndx,Ps) -> plist:set(Key,update_list(Ndx,Val,plist:get(Key,Ps)),Ps) end,
  Nl1 = lists:foldl(fun({Key,Val},A) -> Upd(Key,Val,Doms,A) end, Nl0, Kvs),
  set_nl(Name,Nl1,Nls).


-spec set_wrf_start_time(#nls{},calendar:datetime(),[integer()]) -> #nls{}.
set_wrf_start_time(Nls,{{Y,M,D},{H,Min,S}},Doms) ->
  set_values_for_doms(Nls,"time_control",[{"start_year",Y},{"start_month",M},{"start_day",D},
                                          {"start_hour",H},{"start_minute",Min},{"start_second",S}],Doms).

-spec set_wrf_end_time(#nls{},calendar:datetime(),[integer()]) -> #nls{}.
set_wrf_end_time(Nls,{{Y,M,D},{H,Min,S}},Doms) ->
  set_values_for_doms(Nls,"time_control",[{"end_year",Y},{"end_month",M},{"end_day",D},
                                          {"end_hour",H},{"end_minute",Min},{"end_second",S}],Doms).

-spec set_wrf_run_time(#nls{},calendar:datetime(),calendar:datetime()) -> #nls{}.
set_wrf_run_time(Nls,From,To) ->
  [Ds,Hrs,Mins,Secs] = timelib:days_hrs_mins_secs_between(From,To),
  set_values_for_doms(Nls,"time_control",[{"run_days",Ds},{"run_hours",Hrs},
                                          {"run_minutes",Mins},{"run_seconds", Secs}],[1]).


-spec max_domains(#nls{}) -> integer().
max_domains(Nls) ->
  case has_nl("domains",Nls) of
    true  -> hd(get_entry("domains", "max_dom", Nls));  % it's an input namelist
    false -> hd(get_entry("share", "max_dom", Nls))     % it's a wps namelist
  end.


-spec all_domains(#nls{}) -> [integer()].
all_domains(Nls) -> lists:seq(1, max_domains(Nls)).


-spec set_wps_start_time(#nls{},calendar:datetime(),[integer()]) -> #nls{}.
set_wps_start_time(Nls,DT,Doms) ->
  Esmf = timelib:to_esmf_str(DT),
  set_values_for_doms(Nls,"share",[{"start_date",Esmf}], Doms).


-spec set_wps_end_time(#nls{},calendar:datetime(),[integer()]) -> #nls{}.
set_wps_end_time(Nls,DT,Doms) ->
  Esmf = timelib:to_esmf_str(DT),
  set_values_for_doms(Nls,"share",[{"end_date",Esmf}], Doms).


-spec set_ignition(#nls{},integer(),ign_spec(),integer()) -> #nls{}.
set_ignition(Nls,IgnNdx,IgnSpec,Dom) ->
  Vars = ["fire_ignition_start_lon","fire_ignition_start_lat","fire_ignition_end_lon",
                   "fire_ignition_end_lat","fire_ignition_radius","fire_ignition_start_time",
                   "fire_ignition_end_time"],
  NdxS = integer_to_list(IgnNdx),
  IgnVars = lists:map(fun(X) -> X ++ NdxS end, Vars),
  case IgnSpec of
    {{Lat,Lon},{Tdelta,Tlen,RadiusM}} ->
      Kvs = lists:zip(IgnVars,[Lon,Lat,Lon,Lat,RadiusM,Tdelta,Tdelta+Tlen]),
      set_values_for_dom(Nls,"fire",Kvs,Dom);
    {{Lat1,Lon1},{Lat2,Lon2},{Tdelta,Tlen,RadiusM}} ->
      Kvs = lists:zip(IgnVars,[Lon1,Lat1,Lon2,Lat2,RadiusM,Tdelta,Tdelta+Tlen]),
      set_values_for_dom(Nls,"fire",Kvs,Dom)
  end.

        
-spec set_ignitions(#nls{},[ign_spec()],[integer()]) -> #nls{}.
set_ignitions(Nls0,IgnSpecs,Dom) ->
  Nls1 = set_values_for_dom(Nls0,"fire",[{"fire_num_ignitions",length(IgnSpecs)}],2),
  N = length(IgnSpecs),
  Instr = lists:zip(lists:seq(1,N),IgnSpecs),
  lists:foldl(fun ({I,Is},A) -> set_ignition(A,I,Is,Dom) end, Nls1, Instr).

