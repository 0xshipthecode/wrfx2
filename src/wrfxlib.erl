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

-module(wrfxlib).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-include("wrfx2.hrl").
-export([clone_wps/5,clone_wrf/3,symlink_grib_files/2]).
-export([run_geogrid/2,run_ungrib/2,run_metgrid/2]).
-export([run_real/2,run_real_dmpar/2,run_real_serial/2,run_wrf/11]).


%% @doc Clone the WPS directory <src> to another location <dst>. This function uses a standard list of
%% @doc files and directories that must be symlinked.  As an exception, the geogrid directory is copied
%% @doc entirely, since the geogrid table link therein may need to be overwitten.  The ungrib vtable and
%% @doc a geogrid table are accepted as special arguments."
-spec clone_wps(string(),string(),string(),string(),[string()]) -> ok.
clone_wps(Src,Dst,Vtable,Gtable,WithFiles) ->
  ok = filesys:clone_with_files(Src,Dst,["geogrid.exe","metgrid.exe","metgrid","ungrib.exe","ungrib"]),
  ok = filesys:create_dir(filename:join(Dst,"geogrid")),
  ok = filesys:make_symlink(filename:join(Src,Gtable),filename:join([Dst,"geogrid","GEOGRID.TBL"])),
  ok = filesys:symlink_unless_exists(filename:join(Src,Vtable),filename:join(Dst,"Vtable")),
  filesys:symlink_files_to_dir(WithFiles,Src,Dst),
  ok.


-spec clone_wrf(string(),string(),[string()]) -> ok.
clone_wrf(Src,Dst,WithFiles) ->
  ok = filesys:clone_with_files(Src,Dst,WithFiles ++
    ["CAM_ABS_DATA","CAM_AEROPT_DATA","co2_trans","ETAMPNEW_DATA","ETAMPNEW_DATA_DBL",
     "ETAMPNEW_DATA.expanded_rain","ETAMPNEW_DATA.expanded_rain_DBL","GENPARM.TBL",
     "gribmap.txt","grib2map.tbl","LANDUSE.TBL","MPTABLE.TBL","namelist.fire",
     "ozone.formatted","ozone_lat.formatted","ozone_plev.formatted",
     "real.exe","RRTM_DATA","RRTM_DATA_DBL","RRTMG_LW_DATA","RRTMG_LW_DATA_DBL",
     "RRTMG_SW_DATA","RRTMG_SW_DATA_DBL","SOILPARM.TBL","tc.exe","tr49t67","tr49t85",
     "tr67t85","URBPARM.TBL","URBPARM_UZE.TBL","VEGPARM.TBL","wrf.exe"]),
   ok.


-spec run_geogrid(string(),fun()) -> ok.
run_geogrid(InDir,LogF) ->
  OutSpec = steward_utils:make_std_output_spec(InDir,"geogrid"),
  LogF(info, "running GEOGRID", []),
  ChkF = fun () -> extwrap:exit_check_string_exists(plist:get(1,OutSpec),"Successful completion of geogrid.") end,
  extwrap:run_process("geogrid", "./geogrid.exe", OutSpec, InDir, 1800000, LogF, 0, ChkF, 1),
  LogF(info, "geogrid completed successfully", []),
  ok. 


-spec run_ungrib(string(),fun()) -> ok.
run_ungrib(InDir,LogF) ->
  OutSpec = steward_utils:make_std_output_spec(InDir,"ungrib"),
  LogF(info,"running UNGRIB",[]),
  ChkF = fun () -> extwrap:exit_check_string_exists(plist:get(1,OutSpec),"Successful completion of ungrib.") end,
  extwrap:run_process("ungrib", "./ungrib.exe", OutSpec, InDir, 3600000, LogF, 0, ChkF, 1),
  LogF(info,"ungrib completed succesfully.", []),
  ok.


-spec run_metgrid(string(),fun()) -> ok.
run_metgrid(InDir,LogF) ->
  OutSpec = steward_utils:make_std_output_spec(InDir,"metgrid"),
  LogF(info,"running METGRID",[]),
  ChkF = fun () -> extwrap:exit_check_string_exists(plist:get(1,OutSpec),"Successful completion of metgrid.") end,
  extwrap:run_process("metgrid", "./metgrid.exe", OutSpec, InDir, 3600000, LogF, 0, ChkF, 1),
  LogF(info,"metgrid completed succesfully.", []),
  ok.


-spec run_real_dmpar(string(),fun()) -> ok.
run_real_dmpar(InDir,LogF) ->
  Cmd = filename:join(InDir,"real.exe"),
  OutP0 = filename:join(InDir,"rsl.out.0000"),
  ErrP = filename:join(InDir,"rsl.error.0000"),
  OutP1 = filename:join(InDir,"real.stdout"),
  LogF(info,"running REAL [parallel]",[]),
  ChkFs = extwrap:make_exit_check_seq_f([
    fun () -> filesys:copy_unless_dst(OutP0,OutP1), passed end,
    fun () -> extwrap:exit_check_string_exists(OutP1,"SUCCESS COMPLETE REAL_EM") end]),
  extwrap:run_process("real",Cmd,[],InDir,1800000,LogF,0,ChkFs,1),
  ok = filesys:copy_unless_dst(ErrP,filename:join(InDir,"real.stderr")),
  LogF(info,"real [dmpar] completed succesfully",[]),
  ok.


-spec run_real_serial(string(),fun()) -> ok.
run_real_serial(InDir,LogF) ->
  OutP = filename:join(InDir,"real.stdout"),
  ErrP = filename:join(InDir,"real.stderr"),
  LogF(info,"running REAL [serial]", []),
  ChkF = fun () -> extwrap:exit_check_string_exists(OutP,"SUCCESS COMPLETE REAL_EM") end,
  extwrap:run_process("real","./real.exe",[{1,OutP},{2,ErrP}],InDir,1800000,LogF,0,ChkF,1),
  LogF(info,"real completed succesfully",[]),
  ok.


-spec run_real(string(),fun()) -> ok.
run_real(InDir,LogF) ->
  case configsrv:get_conf(wrf_is_parallel) of
    true  -> run_real_dmpar(InDir,LogF);
    false -> run_real_serial(InDir,LogF)
  end.


-spec next_grib_suffix(list()) -> list().
next_grib_suffix([$Z,$Z,$Z]) -> throw("too many grib files");
next_grib_suffix([S1,$Z,$Z]) -> [S1+1,$A,$A];
next_grib_suffix([S1,S2,$Z]) -> [S1,S2+1,$A];
next_grib_suffix([S1,S2,S3]) -> [S1,S2,S3+1].


-spec symlink_grib_files(list(),string()) -> ok.
symlink_grib_files(Manifest,Dir) ->
  F = fun (X,A) -> filesys:symlink_unless_exists(X,filename:join(Dir,"GRIBFILE."++A)), next_grib_suffix(A) end,
  lists:foldl(F, "AAA", Manifest),
  ok.


-spec monitor_wrf_execution(uuid(),string(),pid(),calendar:datetime(),
                            calendar:datetime(),pid(),fun(),fun(),fun()) -> ok.
monitor_wrf_execution(U,Wdir,ExtMon,SimF,SimT,WrfMon,HistH,CflH,LogF) ->
  receive
    cfl_violation_detected ->
      CflH(ExtMon),
      monitor_wrf_execution(U,Wdir,ExtMon,SimF,SimT,WrfMon,HistH,CflH,LogF);
    {wrf_history_written,DomId,SimT} ->
      HistH(DomId,SimT),
      monitor_wrf_execution(U,Wdir,ExtMon,SimF,SimT,WrfMon,HistH,CflH,LogF);
    {proc_started,ExtMon,StartTS} ->
      LogF(info, "[~p] WRF computation started on ~w, monitoring", [U,StartTS]),
      Fname = filename:join(Wdir,"rsl.error.0000"),
      RealWrfMonPid = 'wrf-monitor':start(Fname,SimF,SimT,StartTS,U,LogF),
      monitor_wrf_execution(U,Wdir,ExtMon,SimF,SimT,RealWrfMonPid,HistH,CflH,LogF);
    {proc_terminated,ExtMon,Result} ->
      case WrfMon of
        undefined -> Result;
        Pid       -> Pid ! terminate, Result
      end;
    {kill,Reason} ->
      LogF(warn, "[~p] received kill request with reason ~p.",[U,Reason]),
      ExtMon ! {kill,Reason},
      monitor_wrf_execution(U,Wdir,ExtMon,SimF,SimT,WrfMon,HistH,CflH,LogF)
  end.


-spec run_wrf(uuid(),string(),integer(),integer(),integer(),calendar:datetime(),calendar:datetime(),
              integer(),fun(),fun(),fun()) -> ok.
run_wrf(U,Wdir,NumNodes,Ppn,WallTimeHrs,SimF,SimT,PidTimeoutS,HistH,CflH,LogF) ->
  WrfCmd = filename:join(Wdir,"wrf.exe"),
  OutSpec = [{1,filename:join(Wdir,"rsl.error.0000")},{1,filename:join(Wdir,"rsl.aux.0000")}],
  JobTimeoutS = (WallTimeHrs + 0.1) * 3600,
  Cfgs = [{hpc_profile, configsrv:get_conf(hpc_profile)}],
  Params = [{task_id,"wrf"},{cmd,WrfCmd},{num_nodes,NumNodes},{proc_per_node,Ppn},{pid_timeout_s,PidTimeoutS},
            {wall_time_hrs,WallTimeHrs},{wall_time_mins,0},{in_dir,Wdir},{job_timeout_s,JobTimeoutS}],
  WrfExec = case configsrv:get_conf(wrf_is_parallel) of
    true -> fun () -> steward_job:execute(Cfgs ++ Params, LogF) end;
    false -> fun () -> steward_process:execute("wrf",WrfCmd,OutSpec,Wdir,JobTimeoutS,LogF) end
  end,
  Result = case WrfExec() of
    {running,ExtMonPid,Qid} ->
      LogF(info,"[~p] WRF [dmpar] is queued with id ~p.", [U,Qid]),
      monitor_wrf_execution(U,Wdir,ExtMonPid,SimF,SimT,undefined,HistH,CflH,LogF);
    {running,ExtMonPid} ->
      LogF(info,"[~p] WRF [serial] has been started.", [U]),
      monitor_wrf_execution(U,Wdir,ExtMonPid,SimF,SimT,undefined,HistH,CflH,LogF);
    Done ->
      Done
  end,
  LogF(info,"[~p] WRF result is ~p", [U,Result]),
  case Result of
    {failure,Reason} -> LogF(error,"[~p] WRF reported failure with reason ~p.", [U,Reason]), throw({failed,Reason});
    {success, 0} -> LogF(info,"[~p] WRF successfully completed.",[U]);
    {success, ExC} -> 
      LogF(warn,"[~p] WRF failed with exit code ~p.",[U,ExC]), 
      throw({failed,wrf_exit_code,ExC});
    {killed,Reason} ->
      LogF(info,"[~p] WRF has been killed with reason ~p", [U,Reason]),
      throw({killed,Reason})
  end,
  Result.

