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
-export([send_package/3,remote_execute/2]).
-include("wrfx2.hrl").


-spec remote_execute(string(),#ssh_endpoint{}) -> any().
remote_execute(What,EndPt) ->
  Cmd = lists:flatten(io_lib:format("ssh ~s ~s ~s", [key_opt(EndPt),url(EndPt),What])),
  utils:log_info("ssh_shuttle remote execution ~n~p~n", [Cmd]),
  os:cmd(Cmd). 


-spec send_package(string(),string(),#ssh_endpoint{}) -> ok|error.
send_package(Path,Dest,EndPt) ->
  Cmd = lists:flatten(io_lib:format("scp ~s ~s ~s >> /dev/null && echo $?", [key_opt(EndPt), Path, url(EndPt,Dest)])),
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
