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


-module(jobmaster).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("wrfx2.hrl").
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([is_live/1,submit/3,live_jobs/0,get_state/1,remove_state/2,update_state/2,append_state/3]).
-export([job_done/2,kill/1]).


-define(SERVER, {global,jobmaster}).


start_link() ->
  true = utils:table_exists("jobs"),
  gen_server:start_link({global,jobmaster},jobmaster,dict:new(),[]),
  gen_server:call(?SERVER, resubmit_live_jobs).


%; --------------------------------------------
%; public API
%; --------------------------------------------

-spec job_done(uuid(),job_status()) -> ok|not_found.
job_done(U,St) -> gen_server:call(?SERVER, {job_done,U,St}).

-spec submit(uuid(),atom(),plist()) -> ok|{error,{missing_keys,[term()]}}.
submit(U,Mod,As) -> gen_server:call(?SERVER, {submit_job,U,Mod,As}).

-spec live_jobs() -> [string()].
live_jobs() -> gen_server:call(?SERVER,list_live_jobs).

-spec is_live(uuid()) -> boolean().
is_live(U) -> gen_server:call(?SERVER,{is_live,U}).

-spec get_state(uuid()) -> plist().
get_state(U) -> gen_server:call(?SERVER,{get_state,U}).

-spec remove_state(uuid(),[string()]) -> ok|not_found.
remove_state(U,Ks) -> gen_server:call(?SERVER,{modify_state,remove,U,Ks}).

-spec update_state(uuid(),plist()) -> ok|not_found.
update_state(U,Ps) -> gen_server:call(?SERVER,{modify_state,update,U,Ps}).

-spec append_state(uuid(),string(),term()) -> ok|not_found.
append_state(U,K,V) -> gen_server:call(?SERVER,{modify_state,append,U,{K,V}}).

-spec kill(string()) -> ok|not_found.
kill(U) ->  gen_server:call(?SERVER,{kill,U,by_request}).


%; --------------------------------------------
%; internal functions
%; --------------------------------------------

-spec execute_job_internal(string(),atom(),plist()) -> pid().
execute_job_internal(U,Mod,As) ->
  logsrv:create_log(U),
  spawn(fun () ->
    try
      Wdir = filename:join(configsrv:get_conf(workspace_dir), U),
      ok = filesys:create_dir(Wdir),
      apply(Mod,run,[[{work_dir,Wdir},{uuid,U}|As], logsrv:make_log_f(U)]),
      job_done(U,completed)
    catch _Exc:{killed,by_request} ->
      utils:log_info("job [~s] was killed by request.",[U]),
      update_state(U,[{note,"killed by request"}]),
      job_done(U,killed);
    _Exc:{killed,Reason} ->
      utils:log_error("job [~s] was killed with reason ~p.~n",[U,Reason]),
      update_state(U,[{note,lists:flatten(io_lib:format("killed with reason ~p", [Reason]))}]),
      job_done(U,killed);
    Exc:Bdy ->
      utils:log_error("job [~s] crashed with exception ~p~n body:~p~n stracktrace:~n~p~n", [U,Exc,Bdy,erlang:get_stacktrace()]),
      update_state(U, [{note,lists:flatten(io_lib:format("crashed with exception type ~p body ~p", [Exc,Bdy]))}]),
      job_done(U,failed)
    end,
    logsrv:close_log(U) end).


-spec resubmit_live_job(#job{}) -> #job{}.
resubmit_live_job(J=#job{uuid=U,module=Mod,args=As}) ->
  utils:log_info("jobmaster: resubmitting live job ~p (module ~p).", [U,Mod]),
  Pid = execute_job_internal(U,Mod,As),
  J#job{pid=Pid}.


% --------------------------------------------
% gen_server callbacks
% --------------------------------------------

init(Args) -> {ok, Args}.


% the jobmaster keeps tabs on all running and ended jobs by keeping
% track of the tuple {uuid job-pid job-state}

handle_call({submit_job,U,Mod,As},_From,LJs) ->
  case plist:find_missing([sim_from,forecast_length_hrs, num_nodes, ppn, grid_code], As) of
    [] ->
      case dict:is_key(U,LJs) of
        true ->
          utils:log_info("jobmaster: did not start job ~p, it is already live", U),
          {reply, ok, LJs};
        false ->
          Pid = execute_job_internal(U,Mod,As),
          ST = calendar:local_time(),
          [SF,FL,NN,PPN,GC] = plist:get_list([sim_from, forecast_length_hrs, num_nodes, ppn, grid_code], As),
          utils:log_info("jobmaster: job ~p is going LIVE now.", [U]),
          J = #job{uuid=U,module=Mod,args=As,status=live,pid=Pid,start_time=ST,end_time=null,grid_code=GC,
                   state=[],sim_from=SF,sim_to=timelib:shift_by(SF,FL,hours),num_nodes=NN,ppn=PPN},
          job:upsert(J),
          catmaster:update_job(J),
          {reply, ok, dict:store(U,J,LJs)}
      end;
    MK ->
      {reply, {error, {missing_keys, MK}}, LJs}
  end;
handle_call({job_done,U,Status},_From,LJs) ->
  utils:log_info("jobmaster: notified that job ~p is done with status ~p", [U,Status]),
  case dict:find(U,LJs) of
    {ok, J} ->
      job:update_status(U,Status),
      catmaster:update_job(J#job{status=Status}),
      {reply, ok, dict:erase(U,LJs)};
    error ->
      utils:log_warn("jobmaster: job ~p not found, not setting status.", [U]),
      {reply, not_found, LJs}
  end;
handle_call({get_state,U},_From,LJs) ->
  case dict:find(U,LJs) of
    {ok, #job{state=S}} -> {reply, S, LJs};
    error               -> {reply, not_found, LJs}
  end;
handle_call({kill,U,Reason},_From,LJs) ->
  case dict:find(U,LJs) of
    {ok, #job{pid=Pid}} ->
      Pid ! {kill, Reason},
      utils:log_info("jobmaster: killing job ~p with reason ~p.", [U,Reason]),
      {reply,ok,LJs};
    error ->
      {reply,not_found,LJs}
  end;
handle_call(list_live_jobs,_From,LJs) -> {reply, dict:fetch_keys(LJs), LJs};
handle_call({is_live,U},_From,LJs) -> {reply, dict:is_key(U,LJs), LJs};
handle_call(resubmit_live_jobs,_From,LJs) ->
  NewJs = lists:map(fun resubmit_live_job/1, job:retrieve_live_jobs()),
  lists:map(fun catmaster:update_job/1, NewJs),
  {reply, ok, lists:foldl(fun(J=#job{uuid=U},Js) -> dict:store(U,J,Js) end, LJs, NewJs)};
handle_call({modify_state,How,U,Arg},_From,LJs) ->
  case dict:find(U,LJs) of
    {ok, J=#job{state=S0}} ->
      S1 = case How of
        remove -> plist:remove_list(Arg,S0);
        update -> plist:update_with(Arg,S0);
        append -> {K,V} = Arg, plist:append(K,V,S0)
      end,
      S2 = plist:set(last_updated,calendar:local_time(),S1),
      J1 = J#job{state=S2},
      job:update_state(U,S2),
      catmaster:update_job(J1),
      {reply, ok, dict:store(U, J1, LJs)};
    error -> {reply, not_found, LJs}
  end;
handle_call(Other,_From,LJs) ->
  utils:log_error("jobmaster: request ~p not understood!", [Other]),
  {reply, invalid_request, LJs}.

handle_cast(_Msg,State) -> {noreply, State}.
handle_info(_Info,State) -> {noreply, State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn,State,_Extra) -> {ok,State}.

