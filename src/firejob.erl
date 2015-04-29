% Copyright (C) 2014-2015 Martin Vejmelka, UC Denver
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

-module(firejob).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run/2,firetest/3]).
-include("wrfx2.hrl").


build_job_uuid(SF,FC) ->
  lists:flatten(["firejob-",timelib:to_esmf_str(SF),"-",io_lib:format("~2..0B", [FC])]).


make_wrf_namelist(SF,ST,NLKeys,HI,AllDoms,IgnSpecs,Nls0) ->
  Nls1 = nllib:make_wrf_namelist(SF,ST,NLKeys,HI,AllDoms,Nls0),
  nlist:set_ignitions(Nls1,IgnSpecs,2).


run(Args,LogF) ->
  [U,IgnSpecs] = plist:get_list([uuid,ign_specs],Args),
  [WorkDir,WpsI,WrfI] = plist:get_list([work_dir,wps_install_dir,wrf_install_dir],Args),
  [GribSrcName,GeogridTbl] = plist:get_list([grib_source_name,geogrid_table],Args),
  [SF,FC,WallTimeHrs] = plist:get_list([sim_from,forecast_length_hrs,wall_time_hrs],Args),
  [NumNodes,Ppn] = plist:get_list([num_nodes,ppn],Args),
  [HI,StationSel,GridCode] = plist:get_list([history_interval,station_selector,grid_code],Args),
  GribInfo = grib_ingest:get_grib_info(GribSrcName),
  Vtable = plist:get(vtable_file,GribInfo),
  ST = timelib:shift_by(SF,FC,hours),
  [WpsW,WrfW,FmdaW] = [filename:join(WorkDir,"wps"),filename:join(WorkDir,"wrf"),filename:join(WorkDir,"fmda")],
  PP = plist:get(postproc, [], Args),
  WpsNl0 = nlist:parse(plist:get(wps_nl,Args),firejob_wps),
  WrfNl0 = nlist:parse(plist:get(wrf_nl,Args),firejob_wrf),
  %FireNl = nlist:parse(plist:get(fire_nl,Args),firejob_fire),
  AllDoms = nlist:all_domains(WpsNl0),
  WrfxDir = configsrv:get_conf(sysdir),
  OnSuccess = plist:get(on_success,not_present,Args),

  LogF(info,"firejob starting: work dir is ~p [wps_inst=~p, vtable=~p, gtable=~p] sim range [~w,~w]~n",
            [WorkDir,WpsI,Vtable,GeogridTbl,SF,ST]),
  ok = wrfxlib:clone_wps(WpsI,WpsW,plist:get(vtable_file,GribInfo),GeogridTbl,[]),

  % step 1: run geogrid or use pre-linked geogrids
  WpsNl1 = nllib:make_geogrid_namelist(Args,WpsNl0),
  WpsPath = filename:join(WpsW,"namelist.wps"),
  LogF(info, "writing namelist.wps for geogrid",[]),
  ok = file:write_file(WpsPath,nlist:render_namelists(WpsNl1)),
  case plist:get(link_geogrids,[],Args) of
    [] ->
      LogF(info,"no precomputed geogrids found, running geogrid.",[]),
      jobmaster:update_state(U,[{stage,"GEOGRID (1/7)"}]),
      perftrack:instrument_as(firejob,U,"geogrid",[],fun () -> wrfxlib:run_geogrid(WpsW,LogF) end);
    PreLinks ->
      LogF(info,"obtained ~p precomputed geogrids, linking and skipping geogrid.",[length(PreLinks)]),
      lists:map(fun ({File,Precomp}) -> 
                  From = filename:join(WrfxDir,Precomp), To = filename:join(WpsW,File), 
                  filesys:symlink_unless_exists(From,To) end, PreLinks)
  end,

  %step 2: retrieve GRIBs and add ungrib-specific entries to namelist and store it
  jobmaster:update_state(U,[{stage,"GRIB retrieval (2/7)"}]),
  LogF(info,"retrieving GRIB files for the simulation extent ~w to ~w",[SF,ST]),
  perftrack:instrument_as(firejob,U,"grib-retrieval",[from,SF,to,ST,grib_src,GribSrcName],
    fun () ->
      Strategy = [{use_grib_source,GribSrcName},try_retrieve,shift_cycle,try_retrieve],
      {success,CovF,CovT,Manifest} = grib_ingest:retrieve_gribs(SF,ST,calendar:universal_time(),Strategy),
      WpsNl2 = nllib:make_ungrib_namelist(CovF,CovT,AllDoms,WpsNl1),
      LogF(info,"received GRIB2 manifest ~p~n",[Manifest]),
      ok = file:write_file(WpsPath,nlist:render_namelists(WpsNl2)),
      wrfxlib:symlink_grib_files(Manifest,WpsW),
      LogF(info,"writing amended namelist.wps for ungrib.exe",[]) end),

  % step 3: UNGRIB
  jobmaster:update_state(U,[{stage,"UNGRIB (3/7)"}]),
  perftrack:instrument_as(firejob,U,"ungrib",[],fun () -> wrfxlib:run_ungrib(WpsW,LogF) end),

  % step 4: METGRID
  jobmaster:update_state(U,[{stage,"METGRID (4/7)"}]),
  perftrack:instrument_as(firejob,U,"metgrid",[],fun () -> wrfxlib:run_metgrid(WpsW,LogF) end),

  % step 5: create wrf dir
  LogF(info,"creating WRF working directory",[]),
  wrfxlib:clone_wrf(WrfI,WrfW,[]),
  %ok = file:write_file(filename:join(WrfW,"namelist.fire"),nlist:render_namelists(FireNl)),

  % step 6: symlink all met_em* files to wrfdir
  MetEmFs = filesys:list_dir_regexp(WpsW, "met_em.+"),
  LogF(info, "symlinking ~p met_em* files to the wrf dir.",[length(MetEmFs)]),
  ok = filesys:symlink_files_to_dir(MetEmFs,WpsW,WrfW),

  % step 7: construct WRF namelist
  WrfNl1 = make_wrf_namelist(SF,ST,plist:get(wrf_namelist_keys,GribInfo),HI,AllDoms,IgnSpecs,WrfNl0),
  LogF(info,"writing namelist.input for WRF",[]),
  ok = file:write_file(filename:join(WrfW,"namelist.input"), nlist:render_namelists(WrfNl1)),

  % step 8: run REAL
  jobmaster:update_state(U, [{stage,"REAL (5/7)"}]),
  perftrack:instrument_as(firejob,U,"real",[],fun () -> wrfxlib:run_real(WrfW,LogF) end),

  % step 8b: render topo/fuels
  postproc:render_domains(WpsW,length(AllDoms),U),

  case StationSel of
    empty_station_selector -> ok;
    SS ->
      jobmaster:update_state(U,[{stage,"REAL (6/7)"}]),
      ok = filesys:clone_with_files(filename:join(WrfxDir,"deps/fmda"),FmdaW,["src","fmda_auto.sh"]),
      Now = timelib:round_hours(timelib:shift_by(calendar:universal_time(),1,hours),down),
      RawsMax = timelib:min_time(Now,ST),
      Now_24 = timelib:shift_by(RawsMax,-24,hours),
      perftrack:instrument_as(firejob,U,"raws-retr",[],
        fun () ->
          LogF(info,"requesting that raws_ingest update observations now (waiting max 4 minutes)",[]),
          raws_ingest:update_now(240),
          LogF(info,"retrieving fm10 RAWS observations for station selector ~p",[SS]),
          Obss = raws_ingest:retrieve_observations(SS,[fm10],{Now_24,RawsMax}),
          LogF(info,"retrieved ~p fm10 observations from ~p to ~p",[length(Obss),Now_24,RawsMax]),
          raws_export:obs_to_csv(Obss,filename:join(FmdaW,"observations")) end),

      LogF(info,"writing configuration for fmda",[]),
      WrfInp = filename:join(WrfW,"wrfinput_d02"),
      FmdaCfg = filename:join(FmdaW,"fmda.cfg"),
      Wksp = configsrv:get_conf(workspace_dir),
      OvrJob = job:find_latest_overlapping_job(SF,GridCode),
      Covars = ["constant","elevation","pressure","temperature","lon","lat","rain"],
      case OvrJob of
        not_found -> fmda:write_fmda_cfg(WrfInp,[],Covars,FmdaCfg);
        #job{uuid=UO,sim_from=SFO} -> 
          WrfoutP = filename:join([Wksp,UO,"wrf","wrfout_d02_"++timelib:to_esmf_str(SFO)]),
          fmda:write_fmda_cfg(WrfInp,WrfoutP,Covars,FmdaCfg)
      end
  end,

  LogF(info,"running fmda (cycling version) now",[]),
  perftrack:instrument_as(firejob,U,"fmda",[],fun () -> 
      extwrap:run_process("fmda","PYTHONPATH=src python src/fmda_cycle.py fmda.cfg",steward_utils:make_std_output_spec(FmdaW,"fmda"),
                          FmdaW,1200000,LogF,0,fun () -> passed end, 1) end),
  
  LogF(info,"running WRF",[]),
  jobmaster:update_state(U,[{stage,"WRF (7/7)"}]),

  HistH = fun (DomId,SN) ->
    postproc:render(U,WrfW,SF,ST,DomId,plist:get(DomId,[],PP)),
    JS = jobmaster:get_state(U),
    CompT = plist:get(completion_time,undefined,JS),
    PercDone = plist:get(percent_done,undefined,JS),
    SimAccel = plist:get(sim_acceleration,undefined,JS),
    LogF(info,"detected history write at ~p with ~w percent done, completion time ~w and acceleration ~w",[SN,PercDone,CompT,SimAccel])
    end,

  CflH = fun (ExtMonPid) -> 
    LogF(error,"*** detected CFL violation, terminating WRF job, ***",[]),
    ExtMonPid ! {kill, cfl_violation_detected} end,

  perftrack:instrument_as(firejob,U,"wrf",[{num_nodes,NumNodes},{ppn,Ppn}],
    fun () -> wrfxlib:run_wrf(U,WrfW,NumNodes,Ppn,WallTimeHrs,SF,ST,120,HistH,CflH,LogF) end),

  case OnSuccess of
    not_present -> ok;
    HookText ->
      jobmaster:update_state(U,[{stage,"SUCCESS HOOK"}]),
      case utils:eval_erlang(HookText,[]) of
        {value,HookF,_} -> HookF();
        Error -> LogF(error,"unable to compile success hook with error ~p",[Error])
      end
  end,

  LogF(info,"end of job reached",[]),
  jobmaster:update_state(U,[{stage,"COMPLETE"},{percent_done,100.0}]),
  ok.


% ign-specs format: [{{lat lon} {ignite_after_seconds ignition_duration_seconds ign-radius-m}} ...]
firetest(SF,FC,IgnS) ->
  U = build_job_uuid(SF,FC),
  jobmaster:submit(U,firejob,[{sim_from,SF},
       {wrf_install_dir,"/share_home/mvejmelka/Packages/wrf-fire.adam/WRFV3/run"},
       {wps_install_dir,"/share_home/mvejmelka/Packages/wrf-fire.adam/WPS"},
       {wps_geog_dir,"/share_home/mvejmelka/Packages/WPS-GEOG"},
       {ign_specs,IgnS},{grid_code,"colorado_fire_2d_v1"}, {geogrid_table,"GEOGRID.TBL_FIRE"},
       {link_geogrids,[{"geo_em.d01.nc","precomputed/colorado-firejob/geo_em.d01.nc"},
                        {"geo_em.d02.nc","precomputed/colorado-firejob/geo_em.d02.nc"}]},
       {postproc,[{2,[{kml,"T2"},{kml,"RH_FIRE"},{kml,"F_ROS"},{kml,"F_INT"},{kml,"FMC_G"},
                      {contour_kml,"FIRE_AREA"},{png,"T2"},{png,"RH_FIRE"},{png,"F_ROS"},{png,"F_INT"},{png,"FMC_G"}]}]},
       {grib_source_name,nam_218}, {wps_nl,"etc/nlists/colfire2d.wps"},{wrf_nl,"etc/nlists/colfire2d.input"},
       {fire_nl,"etc/nlists/colfire.fire"},{station_selector,{region, {37, 41}, {-109.3, -102.2}}},
       {history_interval,[120,60]},{num_nodes,12},{ppn,12},{wall_time_hrs,2},{forecast_length_hrs,FC}]).


