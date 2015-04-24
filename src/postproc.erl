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

-module(postproc).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([render/6]).

%% @doc Render a raster or contour from the instructions in <VarInfos>.
%% @doc Each instruction can be either {V,kml}, {V,png} or {V,contour_kml}, where
%% @doc V is the name of the variable.
render(Uuid, Workdir, SimFrom, SimTime, DomId, VarInfos) ->
  SimTimeS = timelib:to_esmf_str(SimTime),
  SimFromS = timelib:to_esmf_str(SimFrom),
  OutPath = filename:join([configsrv:get_conf('output-dir'),Uuid,SimTimeS]),
  WrfOut = io_lib:format("~s/wrfout_d~2..0B_~s",[Workdir,DomId,SimFromS]),
  filelib:ensure_dir(filename:join(OutPath,"fakefile")),
  lists:map(fun(VI) -> postprocess(WrfOut,OutPath,SimTimeS,DomId,VI) end, VarInfos).


postprocess(WrfOut,OutPath,SimTimeS,DomId,{kml,Var}) ->
  Name = lists:flatten(io_lib:format("~s-~2..0B-~s.kmz",[Var,DomId,SimTimeS])),
  Path = filename:join(OutPath,Name),
  Cmd = lists:flatten(io_lib:format("deps/viswrf/raster2kml.py ~s ~s ~s ~s", [WrfOut,Var,SimTimeS,Path])),
  error_logger:info_msg("postproc: ~p~n", [Cmd]),
  spawn(fun() -> os:cmd(Cmd) end),
  Name;
postprocess(WrfOut,OutPath,SimTimeS,DomId,{png,Var}) ->
  Name = lists:flatten(io_lib:format("~s-~2..0B-~s",[Var,DomId,SimTimeS])),
  Path = filename:join(OutPath,Name),
  Cmd = lists:flatten(io_lib:format("deps/viswrf/raster2png.py ~s ~s ~s ~s", [WrfOut,Var,SimTimeS,Path])),
  error_logger:info_msg("postproc: ~p~n", [Cmd]),
  spawn(fun() -> os:cmd(Cmd) end),
  Name;
postprocess(WrfOut,OutPath,SimTimeS,DomId,{contour_kml,Var}) ->
  Name = lists:flatten(io_lib:format("~s-~2..0B-~s.kml",[Var,DomId,SimTimeS])),
  Path = filename:join(OutPath,Name),
  Cmd = lists:flatten(io_lib:format("deps/viswrf/contour2kml.py ~s ~s ~s ~s", [WrfOut,Var,SimTimeS,Path])),
  error_logger:info_msg("postproc: ~p~n", [Cmd]),
  spawn(fun() -> os:cmd(Cmd) end),
  Name.


