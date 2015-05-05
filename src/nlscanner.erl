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


-module(nlscanner).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([scan/1]).


valid_token(",") -> false;
valid_token("\t") -> false;
valid_token([]) -> false;
valid_token(L) when is_list(L) -> not utils:is_empty(string:strip(L)).

process_token("=",LineNo) -> {'=', LineNo};
process_token("&",LineNo) -> {'&', LineNo};
process_token("/",LineNo) -> {'/', LineNo};
process_token(Chars,LineNo) when is_list(Chars) ->{string,LineNo,Chars}.

process_all_tokens([],_LineNo,Acc) -> lists:reverse(Acc);
process_all_tokens([[$!|_]|_Rest],_LineNo,Acc) -> lists:reverse(Acc);
process_all_tokens([T|Rest],LineNo,Acc) -> process_all_tokens(Rest,LineNo,[process_token(T,LineNo)|Acc]).

scan_lines([],Tokens,LineNo,_RE) -> lists:reverse([{'$end',LineNo}|Tokens]);
scan_lines([L|Rest],Tokens,LineNo,RE) ->
  NewToks = lists:filter(fun valid_token/1, re:split(L,RE,[{return,list}])),
  scan_lines(Rest,[process_all_tokens(NewToks,LineNo,[])|Tokens],LineNo+1,RE).

scan_lines(Ls) ->
  {ok,RE} = re:compile("(/)|(&)|(=)|([-\\w\\.]+)|'([^']*)'|(,)|(\t+)|( +)"),
  lists:flatten(scan_lines(Ls,[],1,RE)).

mark_key_tokens([],Acc) -> lists:reverse(Acc);
mark_key_tokens([{string,LineNo1,Key},{'=',_LineNo2}|Rest],Acc) -> mark_key_tokens(Rest,[{key,LineNo1,Key}|Acc]);
mark_key_tokens([Other|Rest],Acc) -> mark_key_tokens(Rest,[Other|Acc]).

scan(Path) ->
  {ok,Cont} = file:read_file(Path),
  mark_key_tokens(scan_lines(string:tokens(binary_to_list(Cont), "\r\n")),[]).

