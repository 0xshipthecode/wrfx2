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

-module(catmaster).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([export_uuid/1,store_variables/3,remove_uuid/1]).
-export([list_jobs/0,get_state/1,get_job/1,update_job/1,purge_stale_jobs/0,purge_stale_jobs/2]).
-export([start_link/2,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("wrfx2.hrl").

-define(SERVER,{global,catmaster}).

%% -------------------------------------
%% public API
%% -------------------------------------


-spec export_uuid(uuid()) -> ok|error.
export_uuid(U) -> gen_server:call(?SERVER,{export_uuid,U}).

-spec remove_uuid(uuid()) -> ok|error.
remove_uuid(U) -> gen_server:call(?SERVER,{remove_uuid,U}).

-spec store_variables(uuid(),calendar:datetime(),term()) -> ok|error.
store_variables(U,TS,Vs) -> gen_server:call(?SERVER,{store_variables,U,TS,Vs}).

-spec update_job(#job{}) -> ok.
update_job(J) -> gen_server:cast(?SERVER,{update_job,J}), ok.

-spec get_job(uuid()) -> #job{}|not_found.
get_job(U) -> gen_server:call(?SERVER, {get_job,U}).

-spec get_state(uuid()) -> plist()|not_found.
get_state(U) -> gen_server:call(?SERVER, {get_state,U}).

-spec list_jobs() -> [uuid()].
list_jobs() -> gen_server:call(?SERVER, list_jobs).

-spec purge_stale_jobs(pos_integer(),time_unit()) -> ok.
purge_stale_jobs(N,Unit) -> gen_server:handle_call(?SERVER,{purge_stale_jobs,N,Unit}).

-spec purge_stale_jobs() -> ok.
purge_stale_jobs() -> gen_server:handle_call(?SERVER,purge_stale_jobs).


%% -------------------------------------
%% gen_server implementation
%% -------------------------------------

-type job_dict() :: dict:dict(string(),#job{}).
-record(cms, {jdict :: job_dict(), pdir :: string(), keep_days :: integer()}).

-spec start_link(string(), pos_integer()) -> ok.
start_link(Pdir,KeepDays) -> 
  Now = calendar:local_time(),
  Js = job:retrieve_jobs_completed_after(timelib:shift_by(Now, -KeepDays, days)),
  Jd = lists:foldl(fun (J=#job{uuid=U},D) -> dict:store(U,J,D) end, dict:new(), Js),
  gen_server:start_link(?SERVER, catmaster, #cms{jdict=Jd,pdir=Pdir,keep_days=KeepDays}, []).

init(Args) -> {ok, Args}.

handle_call({export_uuid,U}, _From, S=#cms{pdir=D}) -> {reply, export_uuid_int(U,D), S};
handle_call(purge_stale_jobs, _From, S=#cms{jdict=Jd,keep_days=KD}) -> {reply, ok, S#cms{jdict=purge_stale_jobs_int(Jd,KD,days)}};
handle_call({purge_stale_jobs,N,Un}, _From, S=#cms{jdict=Jd}) -> {reply, ok, S#cms{jdict=purge_stale_jobs_int(Jd,N,Un)}};
handle_call({remove_uuid,U}, _From, S=#cms{pdir=D}) -> {reply, remove_uuid_int(U,D), S};
handle_call({store_variables,U,TS,Vs}, _From, S=#cms{pdir=D}) -> {reply, store_vars_int(U,TS,Vs,D), S};
handle_call(list_jobs,_From,S=#cms{jdict=Jd}) -> {reply, dict:fetch_keys(Jd), S};
handle_call({get_state,U},_From,S=#cms{jdict=Jd}) -> {reply, get_state_int(find_job(U,Jd)), S};
handle_call({get_job,U},_From,S=#cms{jdict=Jd}) -> {reply, find_job(U,Jd), S};
handle_call(Invalid,_From,Cfg) -> utils:log_error("catman received an invalid request ~p~n", [Invalid]), {reply, invalid_request, Cfg}.


% functions that must be implemented but are not used in this module
handle_cast({update_job,J=#job{uuid=U}},S=#cms{jdict=Jd0}) -> {noreply, S#cms{jdict=dict:store(U,J,Jd0)}};
handle_cast(_Msg,State) -> {noreply, State}.
handle_info(_Info,State) -> {noreply, State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVer,State,_Extra) -> {ok, State}.


%% -------------------------------------
%% internal functions
%% -------------------------------------

-spec read_json_file(string()) -> {ok,any()}|{error,any()}.
read_json_file(Path) ->
  try
    {ok, B} = file:read_file(Path),
    D = jiffy:decode(B),
    {ok,D}
  catch _Exc:Bdy ->
    {error,Bdy}
  end.


-spec write_json_file(string(),string()) -> ok|{error,any()}.
write_json_file(Path,JSON) ->
  try
    B = jiffy:encode(JSON),
    file:write_file(Path,B),
    ok
  catch _Exc:Bdy ->
    {error,Bdy}
  end.


-spec purge_stale_jobs_int(job_dict(),pos_integer(),time_unit()) -> job_dict().
purge_stale_jobs_int(Jd,N,Un) ->
  MinEndTime = timelib:shift_by(calendar:local_time(),-N,Un),
  dict:filter(fun (#job{end_time=ET}) -> ET > MinEndTime end, Jd).


-spec remove_uuid_int(uuid(),string()) -> ok|error.
remove_uuid_int(_U,_Dir) -> ok.
-spec export_uuid_int(uuid(),string()) -> ok|error.
export_uuid_int(_U,_Dir) -> ok.
-spec store_vars_int(uuid(),string(),any(),string()) -> ok|error.
store_vars_int(_U,_TS,_Vs,_Dir) -> ok.


-spec find_job(uuid(),job_dict()) -> #job{}|not_found.
find_job(U,Jd) ->
  case dict:find(U,Jd) of
    {ok, J} -> J;
    error   -> not_found
  end.

get_state_int(not_found) -> not_found;
get_state_int(#job{state=S}) -> S.

