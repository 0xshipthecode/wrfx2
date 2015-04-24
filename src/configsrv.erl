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

-module(configsrv).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([is_valid_config/1,get_system_dir/0]).
-export([reload_config/0,get_conf/1,get_conf/2,all_keys/0]).
-export([start_link/0,init/1,handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).


-spec is_valid_config(string()) -> true|{false,term()}.
is_valid_config(Path) ->
  case utils:execute_file(Path) of
    {ok, Cfg} -> plist:is_plist(Cfg);
    Error -> {false, Error}
  end.


-spec get_system_dir() -> string().
get_system_dir() ->
  {ok, SysDir} = file:get_cwd(),
  SysDir.


-spec start_link() -> ok.
start_link() ->
  case utils:execute_file("etc/config") of
    {ok,Cfg} -> gen_server:start_link({local, configsrv}, configsrv, [{sysdir, get_system_dir()}|Cfg], []);
    Error -> throw({"Invalid etc/config file", Error})
  end.


-spec reload_config() -> success|any().
reload_config() ->
  case utils:execute_file("etc/config") of
    {ok,Cfg} -> gen_server:call(configsrv, {reload, [{sysdir,get_system_dir()}|Cfg]});
    Error -> Error
  end.


-spec get_conf(atom) -> any().
get_conf(Key) ->
  case gen_server:call(configsrv,{get_conf,Key}) of
    no_such_key -> throw({no_such_key,Key});
    Value -> Value
  end.


-spec get_conf(atom(),term()) -> any().
get_conf(Key,Default) ->
  case gen_server:call(configsrv,{get_conf,Key}) of
    no_such_key -> Default;
    Value -> Value
  end.


-spec all_keys() -> [term()].
all_keys() ->
  gen_server:call(configsrv, all_keys).


% gen_server implementation

init(Args) -> {ok, Args}.

handle_call({get_conf,Key}, _From, Cfg) -> {reply, plist:get(Key,no_such_key,Cfg), Cfg};
handle_call(all_keys,_From,Cfg) ->  io:format("configsrv all_keys has ~p~n", [Cfg]), {reply, plist:keys(Cfg), Cfg};
handle_call({reload,Cfg1},_From,_Cfg) -> {reply, ok, [Cfg1]};
handle_call(stop_cfg_server,_From,Cfg) -> {stop, normal, ok, Cfg};
handle_call(Invalid,_From,Cfg) ->
  error_logger:error_msg("received invalid request ~p", [Invalid]),
  {reply, invalid_request, Cfg}.

handle_cast(_Msg,State) -> {noreply, State}.
handle_info(_Info,State) -> {noreply, State}.
terminate(_Reason,_State) -> ok.
code_change(_OldVer,State,_Extra) -> {ok, State}.

