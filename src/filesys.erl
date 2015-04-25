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

-module(filesys).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([create_dir/1,delete_dir/1,file_type/1,delete/1,symlink_unless_exists/2]).
-export([make_symlink/2,list_dir_regexp/2,delete_regexp_files/2]).
-export([symlink_files_to_dir/2,symlink_files_to_dir/3,clone_with_files/3]).
-export([rename_file/2,copy_file/2,copy_unless_dst/2,move_directory/2]).
-include_lib("kernel/include/file.hrl").


-spec create_dir(string()) -> ok.
create_dir(Dir) ->
  case file:make_dir(Dir) of
    ok             -> ok;
    {error,eexist} -> ok;
    {errlr,Reason} -> throw(Reason)
  end.


-spec delete_dir(string()) -> ok.
delete_dir(Dir) ->
  {ok,Ns} = file:list_dir(Dir),
  lists:foreach(fun (X) -> ok = delete(X) end, Ns),
  ok = file:del_dir(Dir).


-spec file_type(string()) -> undefined|device|directory|other|regular|symlink.
file_type(Name) ->
  case file:read_link_info(Name) of
    {ok,#file_info{type=T}} -> T;
    {error,enoent}          -> enoent
  end.


-spec delete(string()) -> ok.
delete(Path) ->
  case file_type(Path) of
    directory -> ok = delete_dir(Path);
    symlink   -> ok = file:delete(Path);
    regular   -> ok = file:delete(Path)
  end.

 
-spec symlink_unless_exists(string(),string()) -> ok.
symlink_unless_exists(From,To) ->
  true = filelib:is_file(From),
  case file:make_symlink(From,To) of
    ok             -> ok;
    {error,eexist} -> ok;
    Other          -> throw(Other)
  end.


-spec make_symlink(string(),string()) -> ok.
make_symlink(From,To) ->
  true = filelib:is_file(From),
  file:delete(To),  %note: just returns {error,enoent} if the file exists
  file:make_symlink(From,To).


-spec list_dir_regexp(string(),string()) -> ok.
list_dir_regexp(Dir,RE) ->
  {ok,MP} = re:compile(RE),
  {ok,Fs} = file:list_dir(Dir),
  lists:filter(fun (X) -> case re:run(X,MP) of {match,_} -> true; nomatch -> false end end, Fs).


-spec delete_regexp_files(string(),string()) -> ok.
delete_regexp_files(Dir,RE) ->
  lists:foreach(fun (X) -> ok = file:delete(X) end, list_dir_regexp(Dir,RE)).


-spec symlink_files_to_dir([string()],string()) -> ok.
symlink_files_to_dir(Fs,Dir) ->
  F = fun (X) -> ok = symlink_unless_exists(X,filename:join(Dir,filename:basename(X))) end,
  lists:foreach(F, Fs),
  ok.


-spec symlink_files_to_dir([string()],string(),string()) -> ok.
symlink_files_to_dir(Fs,Src,Tgt) ->
  F = fun (X) -> ok = symlink_unless_exists(filename:join(Src,X),filename:join(Tgt,X)) end,
  lists:map(F, Fs),
  ok.


-spec clone_with_files(string(),string(),[string()]) ->ok.
clone_with_files(Src,Dst,Fs) ->
  directory = file_type(Src),
  create_dir(Dst),
  F = fun (X) -> symlink_unless_exists(filename:join(Src,X),filename:join(Dst,X)) end,
  lists:map(F, Fs),
  ok.


-spec rename_file(string(),string()) -> ok.
rename_file(Src,Tgt) -> ok = file:rename(Src,Tgt), ok.


-spec copy_file(string(),string()) -> ok.
copy_file(Src,Dst) -> ok = file:copy(Src,Dst), ok.


-spec copy_unless_dst(string(),string()) -> ok.
copy_unless_dst(Src,Dst) ->
 case filelib:is_file(Src) and not filelib:is_file(Dst) of
   true -> ok = file:copy(Src,Dst)
 end, ok.


-spec move_directory(string(),string()) -> ok.
move_directory(Src,Dst) -> ok = file:rename(Src,Dst), ok.

