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

-module(extwrap).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([run_process/9,exit_check_string_exists/2,make_exit_check_seq_f/1]).

run_process(Tid,Cmd,OutSpec,InDir,Tout,LogF,ReqExitC,ExitCheckF,Tries) ->
  {Tries1,Res} = case steward_process:execute(Tid,Cmd,OutSpec,InDir,Tout,LogF) of
                   {running,Pid} -> {Tries-1,steward_utils:wait_for_completion(Pid)};
                   Finished      -> {Tries,Finished}
                 end,
  RetryPolicy = fun(0,FailExc) -> throw(FailExc);
                   (Non0,_FailExc) -> 
                    steward_utils:remove_execution_files(InDir,Tid),
                    run_process(Tid,Cmd,OutSpec,InDir,Tout,LogF,ReqExitC,ExitCheckF,Non0-1)
                  end,
  case Res of
    {success,ReqExitC} ->
      case ExitCheckF() of
        passed  -> ReqExitC;
        NotPass -> io:format("EXTWRAP ~p ~p~n", [NotPass, erlang:get_stacktrace()]),
                   LogF(warn, "run-process exit check failed for tid ~p with info ~p", [Tid,NotPass]),
                   RetryPolicy(Tries1, {Tid,exit_check_failed,NotPass})
      end;
    {success,OtherExitC} ->
      RetryPolicy(Tries1,{Tid,wrong_exit_code,OtherExitC});
    {killed,WithReason} ->
      steward_utils:remove_execution_files(InDir,Tid),
      throw({killed,WithReason})
  end.


%% @doc Builds a function that performs a sequence of exit checks and returns the first
%% @doc failure, or passed if each function returns passed.
-spec make_exit_check_seq_f([fun()]) -> passed|term().
make_exit_check_seq_f(ExitChkFs) ->
  F = fun(NxtF,Acc) -> case Acc of passed -> NxtF(); NotPassed -> NotPassed end end,
  fun () -> lists:foldl(F, passed, ExitChkFs) end.


%% @doc Check if a string exists in a file.
-spec exit_check_string_exists(string(),string()) -> passed|{missing_string,string()}.
exit_check_string_exists(Path,Msg) ->
  case utils:'grep-file'(Path,Msg) of
    found -> passed;
    _NotF -> {missing_string, Msg}
  end.

