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

-module(logsrv).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("wrfx2.hrl").
-export([start_link/1,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-export([create_log/1,close_log/1,msg/4,make_log_f/1]).

-define(SERVER, {global,wrfx2logsrv}).


%% ---------------------------------
%% public API
%% ---------------------------------

-spec create_log(string()) -> ok|error.
create_log(Name) -> gen_server:call(?SERVER,{create_log,Name}).

-spec close_log(string()) -> ok.
close_log(Name) -> gen_server:call(?SERVER,{close_log,Name}).

-spec msg(string(),log_level(),string(),list()) -> ok.
msg(Name,Lev,Msg,As) ->
  D = get_log_device(Name),
  Txt = construct_message(Lev,Msg,As),
  io:format(D,Txt,[]),
  if Lev >= 2 -> error_logger:error_report(Txt) end.

-spec make_log_f(string()) -> fun().
make_log_f(Name) ->
  D = get_log_device(Name),
  fun(Lev,Msg,As) -> Txt = construct_message(Lev,Msg,As), io:format(D,Txt,[]) end.


%% ---------------------------------
%% gen_server functions
%% ---------------------------------


-spec start_link(string()) -> ok.
start_link(Dir) ->
  gen_server:start_link(?SERVER,logsrv,[Dir,dict:new()],[]),
  ok.

-spec init(any()) -> {ok, any()}.
init(Args) -> {ok, Args}.

handle_call({create_log,Name},_From,State=[Dir,Logs]) ->
  Path = filename:join(Dir, Name ++ ".log"),
  case file:open(Path,[write]) of
    {ok,D} -> {reply, ok, [Dir,dict:store(Name,D,Logs)]};
    error  -> {reply, error, State}
  end;
handle_call({get_log_device,Name},_From,State=[_Dir,Logs]) -> {reply, dict:find(Name,Logs), State};
handle_call({close_log,Name},_From,State=[Dir,Logs]) ->
  case dict:find(Name,Logs) of
    {ok, D} -> file:close(D), {reply, ok, [Dir,dict:erase(Name,Logs)]};
    error   -> {reply, error, State}
  end;
handle_call(Other,_From,State) ->
  error_logger:error_msg("logsrv: invalid request ~p.", [Other]),
  {reply, invalid_request,State}.

handle_cast(_Msg,State) -> {noreply, State}.
handle_info(_Info,State) -> {noreply, State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVsn,State,_Extra) -> {ok,State}.


%% ---------------------------------
%% internal functions
%% ---------------------------------

-spec get_log_device(string()) -> pid().
get_log_device(Name) -> {ok, Log} = gen_server:call(?SERVER,{get_log_device,Name}), Log.

-spec timestamp() -> string().
timestamp() ->
  {{Y,M,D},{H,Min,S}} = calendar:local_time(),
  io_lib:format("~4..0B~2..0B~2..0B:~2..0B~2..0B~2..0B", [Y,M,D,H,Min,S]).

-spec level_string(log_level()) -> string().
level_string(info) ->  "INFO ";
level_string(warn) ->  "WARN ";
level_string(error) -> "ERROR";
level_string(flash) -> "FLASH".

-spec construct_message(log_level(),string(),list()) -> string().
construct_message(Lev,Msg,As) ->
  Text = io_lib:format(Msg,As),
  lists:flatten([timestamp(), " ", level_string(Lev), " ", Text, "\n"]).



