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


-module(wrfx2).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([start/0]).


-spec start_logging() -> ok.
start_logging() ->
  LogDir = configsrv:get_conf(logging_dir),
  filelib:ensure_dir(filename:join(LogDir,"log-file")),
  logsrv:start_link(LogDir),
  ok.


-spec start_ingest_system() -> ok.
start_ingest_system() ->
  raws_ingest:start(),
  application:set_env(grib_ingest,grib_storage_dir,configsrv:get_conf(ingest_dir)),
  grib_ingest:start(),
  ok.


-spec wrfx2web_try_connect() -> ok.
wrfx2web_try_connect() ->
  Name = configsrv:get_conf(wrfx2web_node),
  {ok,Host} = inet:gethostname(),
  NodeName = list_to_atom(lists:flatten([Name,"@",Host])),
  case net_adm:ping(NodeName) of
    pong -> utils:log_info("node ~p is up & connected.", [NodeName]);
    pang -> utils:log_info("node ~p is currently unreachable.", [NodeName])
  end,
  ok.


-spec start_dbase() -> ok.
start_dbase() ->
  [Name,User,Pwd] = configsrv:get_confs([dbname,dbuser,dbpwd]),
  pgsql_manager:start_link(Name,User,Pwd,5).


-spec start() -> ok.
start() ->
  configsrv:start_link(),
  inets:start(),
  crypto:start(),
  utils:log_info("config server keys: ~p", [configsrv:all_keys()]),
  start_dbase(),
  catmaster:start_link(configsrv:get_conf(product_dir),configsrv:get_conf(catalog_keep_days)),
  start_logging(),
  perftrack:initialize(),
  start_ingest_system(),
  jobmaster:start_link(),
  sysmon:start_link(),
  scheduler:start_link(),
  wrfx2web_try_connect(),
  utils:log_info("wrfx2 startup complete",[]).

