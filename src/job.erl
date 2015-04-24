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
-export([retrieve/1,insert/1,update_state/2,update_status/2,find_latest_overlapping_job/2,retrieve_live_jobs/0]).
-include("wrfx2.hrl").

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


-spec sql_to_record([term()]) -> #job{}.
sql_to_record([UuidB,ModB,ArgsB,StatusE,St,Et,SimFrom,SimTo,NNodes,Ppn,GridCode,StateB]) ->
  #job{uuid=binary_to_list(UuidB),module=list_to_atom(binary_to_list(ModB)),
       args=utils:'binary-to-term'(ArgsB),status=enum_to_status(StatusE),
       start_time=St,end_time=Et,sim_from=SimFrom,sim_to=SimTo,num_nodes=NNodes,
       ppn=Ppn,grid_code=binary_to_list(GridCode),state=utils:'binary-to-term'(StateB)}.


-spec retrieve(string()) -> #job{}|not_found|error.
retrieve(Uuid) ->
  Qry = "select (uuid,module,args,status,start_time,end_time,sim_from,sim_to,num_nodes,ppn,grid_code,state) from jobs where uuid = $1",
  case pgsql_manager:extended_query(Qry, [Uuid]) of
    {{select,1}, [Data]} -> sql_to_record(Data);
    {{select,0}, _} -> not_found;
    Error -> 
      error_logger:error_msg("job:retrieve(~p) bad return ~p~n", [Uuid,Error]),
      error
  end.


-spec insert(#job{}) -> ok|error.
insert(J=#job{uuid=U,module=M,args=As,start_time=ST,sim_from=SF,sim_to=ST,num_nodes=NN,ppn=PPN,grid_code=GC}) ->
  Qry = "insert into jobs(uuid,module,args,status,start_time,sim_from,sim_to,num_nodes,ppn,grid_code,state) values($1,$2,$3,'live',$4,$5,$6,$7,$8,$9,'[].')",
  case pgsql_manager:extended_query(Qry, [U,atom_to_list(M),utils:'term-to-string'(As),ST,SF,ST,NN,PPN,GC]) of
    {{insert,1},_} -> ok;
    Error ->
      error_logger:error_msg("job:insert(~p) bad return from pgsql ~p~n", [J,Error]),
      error
  end.


-spec update_state(string(),plist()) -> ok|error.
update_state(Uuid,NewState) ->
  case pgsql_manager:extended_query("update jobs set state=$2 where uuid=$1",[Uuid,NewState]) of
    {{update,1},_} -> ok;
    Error ->
      error_logger:error_msg("job:update_state(~p,~p) bad return from pgsql ~p~n", [Uuid,NewState,Error]),
      error
  end.


-spec update_status(string(),job_status()) -> ok|error.
update_status(Uuid,NewStatus) ->
  Qry = "update jobs set status=$2,end_time=$3 where uuid=$1", 
  case pgsql_manager:extended_query(Qry,[Uuid,status_to_enum(NewStatus),calendar:local_time()]) of
    {{update,1},_} -> ok;
    Error ->
      error_logger:error_msg("job:update_status(~p,~p) bad return from pgsql ~p~n", [Uuid,NewStatus,Error]),
      error
  end.


-spec find_latest_overlapping_job(calendar:datetime(),string()) -> #job{}|not_found.
find_latest_overlapping_job(Ts,GridCode) ->
  Qry = "select (uuid,module,args,status,start_time,end_time,sim_from,sim_to,num_nodes,ppn,grid_code,state) "
        "from jobs where sim_from <= $1 and sim_to >= $1 and grid_code=$2 and status = 'completed' order by sim_from desc",
  case pgsql_manager:extended_query(Qry, [Ts,GridCode]) of
    {{select,0},_} -> not_found;
    {{select,_N},[J|_]} -> sql_to_record(J);
    Error ->
      error_logger:error_msg("job:find_latest_overlapping_job(~p,~p) bad return from pgsql ~p~n", [Ts,GridCode,Error]),
      not_found
  end.

-spec retrieve_live_jobs() -> [#job{}].
retrieve_live_jobs() ->
  Qry = "select (uuid,module,args,status,start_time,end_time,sim_from,sim_to,num_nodes,ppn,grid_code,state) where status = 'live'",
  case pgsql_manager:simple_query(Qry) of
    {{select,_N},Jobs} -> lists:map(fun sql_to_record/1, Jobs);
    Error ->
      error_logger:error_msg("job:retrieve_live_jobs() bad return from pgsql ~p~n", [Error]),
      []
  end.

