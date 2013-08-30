%
%  This is a parser of a list of namelists.
%  (This includes a single/zero namelist situation).
%

Nonterminals namelist_list namelists namelist entries entry values.
Terminals key string '&' '/'.
Rootsymbol namelist_list.
Endsymbol '$end'.

namelist_list -> namelists : '$1'.
namelists -> namelist : ['$1'].
namelists -> namelist namelists : ['$1'|'$2'].

namelist -> '&' string '/' : {element(3,'$2'), []}.
namelist -> '&' string entries '/' : {element(3, '$2'), '$3'}.
entries -> entry : ['$1'].
entries -> entry entries : ['$1'|'$2'].

entry -> key values : {element(3,'$1'), '$2'}.
values -> string : [read_type(element(3, '$1'))].
values -> string values : [read_type(element(3, '$1'))|'$2'].

Erlang code.

read_type(".false.") ->
  false;
read_type(".true.") ->
  true;
read_type(X) ->
  D = lists:foldl(fun (F,Acc) -> try_decode_number(F,X,Acc) end, [], ["~u", "~d", "~f"]),
  case D of
    [] ->
      X;
    [V] ->
      V
    end.


try_decode_number(F,X,[]) ->
  case io_lib:fread(F, X) of
    {ok, [V], []} ->
      [V];
    {ok, [V], "."} ->   % special case for fortan floats "290." with no trailing zero
      [V];
    {ok, _V, _Rest} ->
      [];
    {error, _} ->
      []
    end;
try_decode_number(_F, _X, A) ->
    A.

