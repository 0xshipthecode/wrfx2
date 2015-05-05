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


-module(job).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([retrieve/1,upsert/1,update_state/2,update_status/2,find_latest_overlapping_job/2]).
-export([retrieve_live_jobs/0,retrieve_jobs_completed_after/1]).
-export([uuid/1,sim_from/1]).
-include("wrfx2.hrl").


-spec uuid(#job{}) -> uuid().
uuid(#job{uuid=U}) -> U.


-spec sim_from(#job{}) -> calendar:datetime().
sim_from(#job{sim_from=SF}) -> SF.


-spec retrieve(string()) -> #job{}|not_found|error.
retrieve(Uuid) ->
  Qry = "select (uuid,module,args,status,start_time,end_time,sim_from,sim_to,num_nodes,ppn,grid_code,state) "
     ++ "from jobs where uuid = $1",
  case pgsql_manager:extended_query(Qry, [Uuid]) of
    {{select,1}, [Data]} -> sql_to_record(Data);
    {{select,0}, _} -> not_found;
    {error,E} -> utils:log_error("job:retrieve(~p) bad return ~p~n", [Uuid,E]), error
  end.


-spec upsert(#job{}) -> ok|error.
upsert(J=#job{uuid=U,module=M,args=As,start_time=ST,sim_from=SimF,sim_to=SimT,num_nodes=NN,ppn=PPN,grid_code=GC,state=S}) ->
  Sql = [U,atom_to_list(M),encode_for_sql(As),ST,SimF,SimT,NN,PPN,GC,encode_for_sql(S)],
  QryU = "update jobs set module=$2,args=$3,status='live',start_time=$4,sim_from=$5,sim_to=$6,num_nodes=$7,ppn=$8,grid_code=$9,state=$10 where uuid=$1",
  case pgsql_manager:extended_query(QryU,Sql) of
    {{update,1},_}  -> ok;
    {{update,0},[]} ->
      QryI = "insert into jobs(uuid,module,args,status,start_time,sim_from,sim_to,num_nodes,ppn,grid_code,state) "
          ++ "values($1,$2,$3,'live',$4,$5,$6,$7,$8,$9,$10)",
      case pgsql_manager:extended_query(QryI, Sql) of
        {{insert,0,1},[]} -> ok;
        {error, E}        -> 
          utils:log_error("job:upsert(~p) bad return from insert ~p~n", [J,E]),
          error
      end;
    {error, E} ->
      utils:log_error("job:upsert(~p) bad return from update ~p~n", [J,E]),
      error
  end.


-spec update_state(string(),plist()) -> ok|error.
update_state(Uuid,NewState) ->
  Enc = encode_for_sql(NewState),
  case pgsql_manager:extended_query("update jobs set state=$2 where uuid=$1",[Uuid,Enc]) of
    {{update,1},_} -> ok;
    {{update,0},_} -> utils:log_error("job:update_state(~p,~p): job not in table!", [Uuid,NewState]), ok;
    {error,E} -> utils:log_error("job:update_state(~p,~p) bad return from pgsql ~p~n", [Uuid,NewState,E]), error
  end.


-spec update_status(string(),job_status()) -> ok|error.
update_status(Uuid,NewStatus) ->
  Qry = "update jobs set status=$2,end_time=$3 where uuid=$1", 
  case pgsql_manager:extended_query(Qry,[Uuid,status_to_enum(NewStatus),calendar:local_time()]) of
    {{update,1},_} -> ok;
    {error,E} -> utils:log_error("job:update_status(~p,~p) bad return from pgsql ~p~n", [Uuid,NewStatus,E]), error
  end.


-spec find_latest_overlapping_job(calendar:datetime(),string()) -> #job{}|not_found.
find_latest_overlapping_job(Ts,GridCode) ->
  Qry = "select uuid,module,args,status,start_time,end_time,sim_from,sim_to,num_nodes,ppn,grid_code,state "
        "from jobs where sim_from <= $1 and sim_to >= $1 and grid_code=$2 and status = 'completed' order by sim_from desc",
  case pgsql_manager:extended_query(Qry, [Ts,GridCode]) of
    {{select,0},_} -> not_found;
    {{select,_N},[J|_]} -> sql_to_record(J);
    {error,E} -> utils:log_error("job:find_latest_overlapping_job(~p,~p) bad return from pgsql ~p~n", [Ts,GridCode,E]), not_found
  end.


-spec retrieve_live_jobs() -> [#job{}].
retrieve_live_jobs() ->
  Qry = "select uuid,module,args,status,start_time,end_time,sim_from,sim_to,num_nodes,ppn,grid_code,state from jobs where status = 'live'",
  case pgsql_manager:simple_query(Qry) of
    {{select,_N},Jobs} -> lists:map(fun sql_to_record/1, Jobs);
    {error, E} -> utils:log_error("job:retrieve_live_jobs() bad return from pgsql ~p~n", [E]), []
  end.


-spec retrieve_jobs_completed_after(calendar:datetime()) -> [#job{}].
retrieve_jobs_completed_after(DT) ->
  Qry = "select uuid,module,args,status,start_time,end_time,sim_from,sim_to,num_nodes,ppn,grid_code,state from jobs where end_time > $1",
  case pgsql_manager:extended_query(Qry,[DT]) of
    {{select,_N},Jobs} -> lists:map(fun sql_to_record/1, Jobs);
    {error, E} -> utils:log_error("job:retrieve_jobs_completed_after() bad return from pgsql ~p", [E]), []
  end.


%% -----------------------------------------
%% internal functions
%% -----------------------------------------


-spec sql_to_record({term()}) -> #job{}.
sql_to_record({UuidB,ModB,ArgsB,{job_status,StatusE},St,Et,SimFrom,SimTo,NNodes,Ppn,GridCode,StateB}) ->
  #job{uuid=binary_to_list(UuidB),module=list_to_atom(binary_to_list(ModB)),
       args=utils:binary_to_term(ArgsB),status=enum_to_status(StatusE),
       start_time=St,end_time=Et,sim_from=SimFrom,sim_to=SimTo,num_nodes=NNodes,
       ppn=Ppn,grid_code=binary_to_list(GridCode),state=utils:binary_to_term(StateB)}.


-spec status_to_enum(job_status()) -> binary().
status_to_enum(live) -> <<"live">>;
status_to_enum(failed) -> <<"failed">>;
status_to_enum(killed) -> <<"killed">>;
status_to_enum(completed) -> <<"completed">>.


-spec enum_to_status(binary()) -> job_status().
enum_to_status(<<"live">>) -> live;
enum_to_status(<<"failed">>) -> failed;
enum_to_status(<<"killed">>) -> killed;
enum_to_status(<<"completed">>) -> completed.

-spec encode_for_sql(plist()) -> string().
encode_for_sql(Ps) ->
  lists:flatten(["[", string:join(lists:map(fun encode_pair/1, Ps), ",\n"), "\n]."]).
  
-spec encode_pair({atom(),any()}) -> string().
encode_pair({history_interval, HI}) -> io_lib:format("{history_interval,~w}", [HI]);
encode_pair(T) -> io_lib:format("~p",[T]).




