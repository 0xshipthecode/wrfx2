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


-module(nllib).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([make_geogrid_namelist/2,make_ungrib_namelist/4,make_wrf_namelist/6]).
-include("wrfx2.hrl").


-spec make_geogrid_namelist(plist(),#nls{}) -> #nls{}.
make_geogrid_namelist(Args,Nls0) ->
  GeogDir = plist:get(wps_geog_dir,Args),
  nlist:set_entry("geogrid", "geog_data_path", [GeogDir], Nls0).

-spec make_ungrib_namelist(calendar:datetime(),calendar:datetime(),[integer()],#nls{}) -> #nls{}.
make_ungrib_namelist(CovFrom,CovTo,Doms,Nls0) ->
  Nls1 = nlist:set_wps_start_time(Nls0,CovFrom,Doms),
  Nls2 = nlist:set_wps_end_time(Nls1,CovTo,Doms),
  nlist:set_entry("share","interval_seconds", [3600], Nls2).

-spec make_wrf_namelist(calendar:datetime(),calendar:datetime(),plist(),[integer()],[integer()],#nls{}) -> #nls{}.
make_wrf_namelist(SimF,SimT,GribKeys,HistInt,Doms,Nls0) -> 
  Nls1 = nlist:set_wrf_start_time(Nls0,SimF,Doms),
  Nls2 = nlist:set_wrf_end_time(Nls1,SimT,Doms),
  Nls3 = nlist:set_wrf_run_time(Nls2,SimF,SimT),
  Nls4 = nlist:set_entries(GribKeys,Nls3),
  Nls5 = nlist:set_entry("time_control","interval_seconds",[3600],Nls4),
  nlist:set_entry("time_control", "history_interval", HistInt, Nls5).

