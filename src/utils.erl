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
%

-module(utils).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([is_empty/1,wait_for_file/3,eval_erlang/2,grep_file/2,make_uuid/0,str_to_num/1]).
-export([wait_for_task_set/1,term_to_string/1,string_to_term/1,binary_to_term/1]).
-export([log_info/1,log_info/2,log_warn/1,log_warn/2,log_error/1,log_error/2]).
-export([table_exists/1,preflight_check/1]).

-include("wrfx2.hrl").

-spec is_empty(list()) -> boolean().
is_empty([]) -> true;
is_empty(L) when is_list(L) -> false.


-spec str_to_num(list()) -> number().
str_to_num(Lst) ->
  try
    list_to_float(Lst)
  catch _Exc:_Bdy ->
    list_to_integer(Lst)
  end.


wait_for_file(Path,TimeoutMs,WaitChunkMs) ->
  case filelib:is_regular(Path) of
    true -> {success,Path};
    false ->
      case TimeoutMs > WaitChunkMs of
        true ->
          timer:sleep(WaitChunkMs),
          wait_for_file(Path,TimeoutMs-WaitChunkMs,WaitChunkMs);
        false ->
          {failure,timeout}
      end
  end.


% plist() is actually a binding_struct() but should be same
-spec eval_erlang(string(),plist()) -> {value,any(),plist()}.
eval_erlang(Str,Env) ->
  case erl_scan:string(Str) of
    {ok,Scanned,_} ->
      case erl_parse:parse_exprs(Scanned) of
        {ok,Parsed} -> erl_eval:exprs(Parsed,Env);
        {error,Info} -> {error,parse,Info}
      end;
    {error,Info,Loc} -> {error,scan,{Info,Loc}}
  end.


% first param is actually an io_device(), where is it defined?
-spec grep_device(term(),string()) -> found|not_found|error.
grep_device(D,Target) ->
  case file:read_line(D) of
    eof -> not_found;
    {error,_} -> error;
    {ok,L} ->
      case string:str(L,Target) of
        0 -> grep_device(D,Target);
        _ -> found
      end
  end.
              
-spec grep_file(string(),string()) -> found|not_found|error.
grep_file(Path,Target) ->
  {ok,D} = file:open(Path,[read]),
  Res = grep_device(D,Target),
  file:close(D),
  Res.


-spec make_uuid() -> string().
make_uuid() ->
  <<A:32,B:15,C:16,C:16,D:16,E:48>> = crypto:rand_bytes(16),
  lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-4~3.16.0b-~4.16.0b-~12.16.0b",
    [A,B,C band 4095,(D band 16383) bor 32768,E])).


-spec wait_for_task_set(list(),list()) -> plist().
wait_for_task_set(Keys,Results) ->
  case Keys of
    [] -> Results;
    _ ->
      receive
        {kill,Reason} -> throw({killed,Reason});
        {Key,Result} ->
          case lists:member(Key,Keys) of
            true -> wait_for_task_set(lists:delete(Key,Keys),[{Key,Result}|Results]);
            false -> throw({invalid_key_in_wait,Key})
          end
      end
  end.

-spec wait_for_task_set(list()) -> plist().
wait_for_task_set(Keys) -> wait_for_task_set(Keys,[]).

-spec term_to_string(term()) -> string().
term_to_string(Et) -> io_lib:format("~p.", [Et]).

-spec string_to_term(string()) -> any().
string_to_term(S) -> {value,Et,_} = eval_erlang(S,erl_eval:new_bindings()), Et.

-spec binary_to_term(binary()) -> any().
binary_to_term(B) -> string_to_term(binary_to_list(B)).

-spec log_info(any()) -> ok.
log_info(As) when is_list(As) -> error_logger:info_report(lists:flatten(As));
log_info(As) -> error_logger:info_report(As).

-spec log_info(string(),list()) -> ok.
log_info(Fmt,As) -> log_info(io_lib:format(Fmt,As)).

-spec log_warn(any()) -> ok.
log_warn(As) when is_list(As) -> error_logger:warning_report(lists:flatten(As));
log_warn(As) -> error_logger:warning_report(As).

-spec log_warn(string(),list()) -> ok.
log_warn(Fmt,As) -> log_warn(io_lib:format(Fmt,As)).

-spec log_error(any()) -> ok.
log_error(As) when is_list(As) -> error_logger:error_report(lists:flatten(As));
log_error(As) -> error_logger:error_report(As).

-spec log_error(string(),list()) -> ok.
log_error(Fmt,As) -> log_error(io_lib:format(Fmt,As)).

-spec table_exists(string()) -> boolean().
table_exists(Name) ->
  case pgsql_manager:extended_query("select * from information_schema.tables where table_name = $1", Name) of
    {{select,1},_} -> true;
    _              -> false
  end.

-spec preflight_check(plist()) -> list().
preflight_check([]) -> [];
preflight_check([{dir_exists,Path,Err}|Rest]) ->
  case filelib:is_dir(Path) of
    true -> preflight_check(Rest);
    false -> [{Err,Path}|preflight_check(Rest)]
  end;
preflight_check([{file_exists,Path,Err}|Rest]) ->
  case filelib:is_regular(Path) of
    true -> preflight_check(Rest);
    false -> [{Err,Path}|preflight_check(Rest)]
  end;
preflight_check([{is_pid,P,Err}|Rest]) ->
  case is_pid(P) of
    true -> preflight_check(Rest);
    false -> [{Err,P}|preflight_check(Rest)]
  end;
preflight_check([{positive_integer,Int,Err}|Rest]) ->
  case is_integer(Int) and Int > 0 of
    true -> preflight_check(Rest);
    false -> [{Err,Int}|preflight_check(Rest)]
  end.

