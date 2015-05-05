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

-module(wrf_monitor).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/6]).
-include("wrfx2.hrl").

-spec estimate_remaining_time(calendar:datetime(),calendar:datetime(),number()) -> number().
estimate_remaining_time(SimN,SimT,SimAcc) ->
  SimTimeLeft = timelib:seconds_between(SimN,SimT),
  round(SimTimeLeft / SimAcc).


-spec estimate_acceleration(calendar:datetime(),calendar:datetime(),calendar:datetime(),calendar:datetime()) -> number().
estimate_acceleration(SimF,SimN,CompS,CompN) ->
  SimS = timelib:seconds_between(SimF,SimN),
  ComS = timelib:seconds_between(CompS,CompN),
  SimS / ComS.


-spec percent_complete(calendar:datetime(),calendar:datetime(),calendar:datetime()) -> number().
percent_complete(SimF,SimN,SimT) ->
  DoneTime = timelib:seconds_between(SimF,SimN),
  TotalTime = timelib:seconds_between(SimF,SimT),
  100.0 * DoneTime / TotalTime.


-record(wms,{dev,simf,simt,comps,uuid,pid,logf}).

-spec process_timing(#wms{},string()) -> ok.
process_timing(#wms{uuid=U,simf=SF,simt=ST,comps=CS}, L) ->
  TimeNdx = string:str(L,"time"),
  EsmfDate = string:substr(L,TimeNdx+5,19),
  DomNdx = string:str(L,"domain"),
  DomId = list_to_integer(string:strip(string:substr(L,DomNdx + 6,4))),
  case DomId of
    1 ->
      Now = calendar:local_time(),
      SimN = timelib:parse_esmf(EsmfDate),
      case Now of
        CS -> ok;  % don't compute anything on the first write (division by 0)
        _Later ->
          Accel = estimate_acceleration(SF,ST,CS,Now),
          ToGo  = estimate_remaining_time(SimN,ST,Accel),
          PercDone = percent_complete(SF,SimN,ST),
          jobmaster:update_state(U, [{completion_time,timelib:shift_by(Now, ToGo, seconds)},
                                     {seconds_to_finish,ToGo},{sim_acceleration,Accel},
                                     {sim_time,SimN},{percent_done,PercDone}]),
          ok
      end;
    _ -> ok
  end.


-spec process_writing(#wms{},string()) -> ok.
process_writing(#wms{uuid=U,pid=Pid},L) ->
  EsmfDate = string:substr(L,31,19),
  DomId = list_to_integer(string:strip(string:substr(L,61,9))),
  SimN  = timelib:parse_esmf(EsmfDate),
  jobmaster:update_state(U,[{wrf_history_written,{DomId,SimN}}]),
  Pid ! {wrf_history_written,DomId,SimN},
  ok.


-spec monitor_loop(#wms{}) -> ok.
monitor_loop(W=#wms{dev=D,pid=Pid,logf=LogF}) ->
  try
    case file:read_line(D) of
      eof ->
        receive
        after 250 -> monitor_loop(W), {terminate, ok}
        end;
      {ok,Line} ->
        case lists:prefix("Timing for main", Line) of
          true -> process_timing(W,Line);
          false -> ok
        end,
        case lists:prefix("Timing for Writing", Line) of
          true -> process_writing(W,Line);
          false -> ok
        end,
        case string:str(Line, "cfl") > 0 of
          true -> Pid ! cfl_violation_detected;
          false -> ok
        end,
        monitor_loop(W);
      {error,Reason} ->
        utils:log_error("wrf_monitor encountered an error [~p] reading from rsl.error.0000", [Reason])
    end
  catch Exc:Bdy ->
    LogF(flash,"wrf_monitor encountered exception ~p:~p~n stacktrace ~p~n", [Exc,Bdy,erlang:get_stacktrace()]),
    monitor_loop(W)
  end.
           

-spec run(string(),calendar:datetime(),calendar:datetime(),calendar:datetime(),uuid(),pid(),fun()) -> {success,term()}|{failure,timeout}.
run(Path,SF,ST,CS,U,Pid,LogF) ->
  case utils:wait_for_file(Path,120000,500) of
    {success,_} ->
      {ok,D} = file:open(Path,[read]),
      monitor_loop(#wms{dev=D,simf=SF,simt=ST,comps=CS,uuid=U,pid=Pid,logf=LogF}),
      file:close(D);
    {failure,timeout} ->
      LogF(error, "wrf_monitor waited 120 seconds for file ~p to appear and timed out.", [Path]),
      {failure,timeout}
  end.


-spec start(string(),calendar:datetime(),calendar:datetime(),calendar:datetime(),uuid(),fun()) -> pid().
start(Path,SimF,SimT,CompS,U,LogF) ->
  Me = self(),
  spawn(fun () -> run(Path,SimF,SimT,CompS,U,Me,LogF) end).



