% Copyright (C) 2013 Martin Vejmelka, UC Denver
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

-module(testjob).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run/2,test/0]).


run(Args,LogF) ->
  [U,WorkDir] = plist:get_list([uuid,work_dir],Args),
  Now = timelib:round_hours(calendar:universal_time(),down),
  LogF(info, "testjob [~s] with work-dir ~s", [U,WorkDir]),
  jobmaster:update_state(U,[{stage,"BEGIN"}]),
  perftrack:instrument_as("testjob", U,"grib-retr",[],
    fun() -> grib_ingest:retrieve_gribs(Now,timelib:shift_by(Now,4,hours),Now,
              [{use_grib_source,nam_218},try_retrieve,shift_cycle,try_retrieve]) end),

  perftrack:instrument_as("testjob",U,"task1",[],
    fun() -> extwrap:run_process("task1","sleep 5",[],WorkDir,6000,LogF,0,fun () -> passed end,1) end),

  jobmaster:update_state(U,[{stage,"stage 2"}]),

  perftrack:instrument_as("testjob",U,"task2",[],
    fun() -> extwrap:run_process("task2","sleep 5",[],WorkDir,6000,LogF,0,fun() -> passed end,1) end),

  LogF(info,"testjob [~s] end of job reached",[U]),
  jobmaster:update_state(U,[{stage,"COMPLETE"}]),

  ok.

test() ->
  U = utils:make_uuid(),
  jobmaster:submit(U,testjob,[{sim_from, {{2015,4,23},{0,0,0}}}, {grid_code, "dummy_grid"},
                              {forecast_length_hrs,1}, {num_nodes,1}, {ppn,1}]).

