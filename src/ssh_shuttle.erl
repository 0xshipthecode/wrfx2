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

-module(ssh_shuttle).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([pack_tree/2,send_file/3,remote_execute/2,send_package/5]).
-include("wrfx2.hrl").


% NOTE: there is a caveat with filenames containing colons. scp needs to be forced
% to interpret them as local files (it tries to resolve them into host:path) by e.g.
% writing an absolute path or relative prefixed by ./
-spec send_package(string(),string(),string(),string(),#ssh_endpoint{}) -> ok|error.
send_package(Tree,PackFile,RemotePath,RemoteScript,EndPt) ->
  pack_tree(PackFile,Tree),
  send_file(PackFile,RemotePath,EndPt),
  file:delete(PackFile), % this could be optional
  case RemoteScript of
    [] -> ok;
    RealScript -> remote_execute(RealScript, EndPt), ok
  end.


-spec pack_tree(string(),string()) -> ok|error.
pack_tree(ArchiveP,Tree) ->
  ParDir = filename:dirname(Tree),
  Last = filename:basename(Tree),
  Cmd = lists:flatten(io_lib:format("tar -cvjf ~s -C ~p ~s", [ArchiveP,ParDir,Last])),
  os:cmd(Cmd).



-spec remote_execute(string(),#ssh_endpoint{}) -> any().
remote_execute(What,EndPt) ->
  Cmd = lists:flatten(io_lib:format("ssh ~s ~s ~s", [key_opt(EndPt),url(EndPt),What])),
  utils:log_info("ssh_shuttle remote execution ~n~p~n", [Cmd]),
  os:cmd(Cmd). 


% NOTE: there is a caveat with filenames containing colons. scp needs to be forced
% to interpret these as relative (see fix_path/1).
-spec send_file(string(),string(),#ssh_endpoint{}) -> ok|error.
send_file(Path,Dest,EndPt) ->
  Cmd = lists:flatten(io_lib:format("scp ~s ~s ~s >> /dev/null && echo $?", [key_opt(EndPt), fix_path(Path), url(EndPt,Dest)])),
  utils:log_info("ssh_shuttle sending package ~p as ~n~p~n", [Path, Cmd]),
  case os:cmd(Cmd) of
    "0\n" -> ok;
    _     -> error
  end.


-spec key_opt(#ssh_endpoint{}) -> list().
key_opt(#ssh_endpoint{ssh_key=default}) -> [];
key_opt(#ssh_endpoint{ssh_key=KeyF}) -> ["-i ",  KeyF].


-spec url(#ssh_endpoint{},string()) -> list().
url(#ssh_endpoint{user=U,host=H},P) -> [U,"@",H,":",P].

-spec url(#ssh_endpoint{}) -> list().
url(#ssh_endpoint{user=U,host=H}) -> [U,"@",H].


% NOTE: there is a caveat with filenames containing colons. scp needs to be forced
% to interpret them as local files (it tries to resolve them into host:path) by e.g.
% writing an absolute path or relative prefixed by ./
-spec fix_path(string()) -> string().
fix_path(P=[$/|_]) -> P;
fix_path(P) -> "./" ++ P.

