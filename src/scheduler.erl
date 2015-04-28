% Copyright (C) 2013-2014 Martin Vejmelka, UC Denver
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


-module(scheduler).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([reload_schedule/0,get_schedule/0]).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("wrfx2.hrl").


-define(SERVER,wrfx2scheduler).


-spec reload_schedule() -> integer().
reload_schedule() -> 
  Js = load_schedule(),
  gen_server:call(?SERVER, {reload_schedule,Js}, 5000).


-spec get_schedule() -> [#schedule{}].
get_schedule() -> gen_server:call(?SERVER, get_schedule).


-spec start_link() -> ok.
start_link() ->
  true = utils:'table-exists?'("schedule"),
  {ok,_Pid} = gen_server:start_link({local,?SERVER},scheduler,[calendar:local_time(),[],[]], []),
  utils:log_info("scheduler: pre-init ok, loading schedules ...", []),
  N = reload_schedule(),
  utils:log_info("schedule: ~p schedules loaded succesfully", [N]).


%% -----------------------------------
%% gen_server callbacks
%% -----------------------------------


init(Args) -> {ok,Args,5000}.


handle_call({reload_schedule,Js},_From,[Last={_,LastT},_,_]) ->
  {Past,Todo} = lists:partition(fun(#schedule{start_time=ST}) -> ST < LastT end, Js),
  {reply, length(Js), [Last,Past,sort_jobs(Todo)], 5000};
handle_call(get_schedule,_From,S=[_,Past,Todo]) -> {reply, Past ++ Todo, S};
handle_call(Other,_From,State) ->
  utils:log_error("scheduler: invalid request ~p", [Other]),
  {reply, invalid_request, State}.


handle_info(timeout,[Last,Past0,Todo0]) ->
  Now = calendar:local_time(),
  {Past1,Todo1} = start_jobs(Last,Now,Past0,Todo0),
  {noreply,[Now,Past1,Todo1],5000};
handle_info(Other,State) ->
  utils:log_error("scheduler: received invalid info request ~p", [Other]),
  {noreply,State,5000}.


handle_cast(_Msg,State) -> {noreply,State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn,State,_Extra) -> {ok,State}.


%% -----------------------------------
%% internal API
%% -----------------------------------


-spec load_schedule() -> [#schedule{}].
load_schedule() ->
  case pgsql_manager:simple_query("select job_name,start_time,function from schedule") of
    {{select, _N}, Data} -> lists:map(fun sql_to_schedule/1, Data);
    Error                -> utils:log_error("scheduler failed to load schedules with error ~p", [Error]), []
  end.


-spec sql_to_schedule(list()) -> #schedule{}.
sql_to_schedule({JNB,ST,FunB}) ->
  {ok,Scanned,_} = erl_scan:string(binary_to_list(FunB)),
  {ok,Parsed} = erl_parse:parse_exprs(Scanned),
  {value,F,_Bindings} = erl_eval:exprs(Parsed,[]),
  #schedule{job_name=binary_to_list(JNB),start_time=ST,function=F}.


-spec sort_jobs([#schedule{}]) -> [#schedule{}].
sort_jobs(Js) -> lists:sort(fun(#schedule{start_time=ST1},#schedule{start_time=ST2}) -> ST1 < ST2 end, Js).


-spec start_jobs(calendar:time(),calendar:time(),[#schedule{}],[#schedule{}]) -> {[#schedule{}],[#schedule{}]}.
start_jobs(L={LastD,LastT},N={NowD,NowT},Past,Todo) ->
  case NowD of
    LastD -> % we have already called start_jobs(..) today
      case Todo of
        [] -> {Past, []};  % nothing to do
        [J=#schedule{start_time=ST}|TodoRest] ->  % at least one more job to do
          case ST < NowT andalso ST > LastT of
            true -> exec_job(J,N), start_jobs(L,N,[J|Past],TodoRest);
            false -> {Past,Todo}
          end
      end;
    _NewDay ->
      utils:log_info("scheduler: rolling over to a new day ~p", [N]),
      start_jobs({NowD,{0,0,0}}, {NowD,NowT}, [], sort_jobs(Past ++ Todo))
 end.


-spec exec_job(#schedule{},calendar:datetime()) -> pid().
exec_job(#schedule{job_name=JN,start_time=ST,function=F},Now) ->
  utils:log_info("scheduler: starting job ~p at ~w, start time was ~w.", [JN,Now,ST]),
  SF = fun () ->
         try
           F()
         catch Exc:Bdy ->
           utils:log_error("scheduler: job ~p threw exception ~w:~w~nstacktrace: ~p~n", 
                           [JN,Exc,Bdy,erlang:get_stacktrace()])
         end
       end,
  spawn(SF).

