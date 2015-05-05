% Copyright (C) 2015 Martin Vejmelka, UC Denver
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
%

-module(perftrack).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([initialize/0,instrument_as/4,instrument_as/5]).
-include("wrfx2.hrl").


-spec initialize() -> ok.
initialize() ->
  true = utils:table_exists("taskinfo"),
  utils:log_info("perftrack: initialized, tables available.", []),
  ok.


-spec insert_task(atom(),string(),string(),calendar:datetime(),calendar:datetime(),term(),term()) -> term().
insert_task(JT,U,TN,ST,ET,Res,Info) ->
  Qry = "insert into taskinfo(uuid,job_type,task_name,start_time,end_time,result,info) "
     ++ "values ($1,$2,$3,$4,$5,$6,$7)",
  Params = [U,atom_to_list(JT),TN,ST,ET,utils:term_to_string(Res),utils:term_to_string(Info)],
  pgsql_manager:extended_query(Qry,Params).


-spec instrument_as(atom(),string(),string(),term(),fun()) -> term().
instrument_as(JT,U,TN,I,F) ->
  ST = calendar:local_time(),
  try
    Res = F(),
    insert_task(JT,U,TN,ST,calendar:local_time(),{returned,Res},I),
    Res
  catch Exc:Bdy ->
    insert_task(JT,U,TN,ST,calendar:local_time(),{exception,Exc,Bdy},I),
    error_logger:error_msg("task ~p threw exception ~p:~p~nstracktrace ~p~n",
                           [TN,Exc,Bdy,erlang:get_stacktrace()]),
    throw(Bdy)
  end.


-spec instrument_as(atom(),string(),string(),fun()) -> term().
instrument_as(JT,U,TN,F) -> instrument_as(JT,U,TN,[],F).

