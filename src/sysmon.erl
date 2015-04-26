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

-module(sysmon).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([get_state/0]).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).
-include("wrfx2.hrl").

-define(SERVER,sysmon).

-record(sms, {commands,state}).


-spec start_link() -> ok.
start_link() ->
  Host = os:cmd("hostname"),
  Now = calendar:local_time(),
  Cmds = configsrv:get_confs([sysdiag_get_nodes, sysdiag_get_free_nodes, sysdiag_qlen]),
  S = #sms{commands=Cmds, state=[{host,Host},{last_updated,Now}]},
  case gen_server:start_link({global,?SERVER}, sysmon, S, []) of
    {ok, _Pid} -> ok;
    Error      -> throw({"error starting sysmon", Error})
  end.
  

-spec get_state() -> plist().
get_state() -> gen_server:call(?SERVER,get_state).

%% -----------------------------------------
%% gen_server functions
%% -----------------------------------------

-spec init([term()]) -> {ok,[term()]}.
init(Args) -> erlang:send_after(10000, self(), update_state), {ok, Args}.

handle_call(get_state,_From,S={state=SS}) -> {reply, SS, S};
handle_call(Other,_From,S) -> utils:log_error("invalid request to sysmon ~p", [Other]), {reply, invalid_request, S}.

handle_cast(_Msg,State) -> {noreply,State}.

handle_info(update_state,S=#sms{state=SS0}) ->
  erlang:send_after(10000, self(), update_state),
  {noreply, S#sms{state=update_state(SS0)}};
handle_info(_Other,S) -> {noreply, S}.

terminate(_Reason,_State) -> ok.
code_change(_OldVsn,S,_Extra) -> {ok,S}.

%% -----------------------------------------
%% Internal functions
%% -----------------------------------------


-spec get_cmd_output(string()) -> string().
get_cmd_output(Cmd) -> string:strip(os:cmd(Cmd), right, $\n).

-spec update_state(plist()) -> plist().
update_state({Cmds,S}) ->
  try
    [NN,FN,QL] = lists:map(fun (Cmd) -> list_to_integer(get_cmd_output(Cmd)) end, Cmds),
    {Cmds, plist:update_with([{nodes,NN},{freenodes,FN},{qlen,QL},{last_updated,calendar:local_time()}],S)}
  catch Exc:Bdy ->
    utils:log_error("sysmon: failed to update state, exception ~p:~p, stack trace ~p~n", [Exc,Bdy,erlang:get_stacktrace()]),
    S
  end.


