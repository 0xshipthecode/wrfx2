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

-module(plist).
-author("Martin Vejmelka <vejmelkam@gmail.com>").
-export([new/0,is_plist/1,keys/1,contains/2,find_missing/2,remove/2,remove_list/2]).
-export([set/3,get/2,get/3,get_list/2,get_list/3,update_with/2,store/2,load/1]).


-type plist() :: [{term(),term()}].


-spec new() -> [].
new() -> [].


-spec is_plist(term()) -> boolean().
is_plist([]) -> true;
is_plist([{_Key,_Value}|Rest]) -> is_plist(Rest);
is_plist(_) -> false.


-spec keys(plist()) -> [term()].
keys(Ps) -> lists:map(fun ({K,_V}) -> K end, Ps).


-spec set(term(), term(), plist()) -> plist().
set(K,V,[]) -> [{K,V}];
set(K,V,[{K,_V0}|Ps]) -> [{K,V}|Ps];
set(K,V,[O|Ps]) -> [O|set(K,V,Ps)].


-spec get(term(), plist()) -> term().
get(K,[]) -> throw({no_such_key, K});
get(K,[{K,V}|_Ps]) -> V;
get(K,[_O|Ps]) -> get(K,Ps).


%% @doc Returns default value if key is not found.
-spec get(term(), term(), plist()) -> term().
get(_K,D,[]) -> D;
get(K,_D,[{K,V}|_Ps]) -> V;
get(K,D,[_O|Ps]) -> get(K,D,Ps).


%% @doc retrieve a list of values in order of keys <Ks>.
get_list(Ks,Ps) -> lists:map(fun (K) -> get(K, Ps) end, Ks).


%% @doc retrieve a list of values in order of keys <Ks>,
%% @doc substitute defaults <Ds> if not found.
get_list(Ks,D,Ps) -> list:map(fun (K) -> get(K,D,Ps) end, Ks).


-spec contains(term(),plist()) -> boolean().
contains(_K,[]) -> false;
contains(K,[{K,_V}|_Ps]) -> true;
contains(K,[_O|Ps]) -> contains(K,Ps).


-spec find_missing(list(),plist()) -> list().
find_missing(Ks,Ps) ->
lists:foldl(fun(K,L) -> case contains(K,Ps) of true -> L; false -> [K|L] end end, [], Ks).


-spec remove(term(),plist()) -> plist().
remove(_K,[]) -> [];
remove(K,[{K,_V}|Ps]) -> Ps;
remove(K,[O|Ps]) -> [O|remove(K,Ps)].


-spec remove_list(list(),plist()) -> plist().
remove_list(Ks,Ps) -> lists:foldl(fun({K,PsA}) -> remove(K,PsA) end, Ps, Ks).


-spec update_with(plist(),plist()) -> plist().
update_with(Ps1,Ps0) -> lists:foldl(fun({K,V},Acc) -> set(K,V,Acc) end, Ps0, Ps1).


-spec store(string(),plist()) -> ok|{error,term()}.
store(Path,Ps) ->
  {ok,D} = file:open(Path, [write]),
  lists:map(fun(V) -> io:format(D, "~p.~n", [V]) end, Ps),
  file:close(D).


-spec load(string()) -> plist()|{error,term()}.
load(Path) ->
  case file:consult(Path) of
    {ok,Ps} -> Ps;
    E -> E
  end.


