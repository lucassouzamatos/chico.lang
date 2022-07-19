-module(chico_type_checker).

-export([check/1]).

-record(chico_type_env, {
  vars = []
}).

-define(is_var, {variable, _, _}).

empty_type_env() -> #chico_type_env{vars=[]}.

check([]) -> [];
check([T]) -> [visit(T, empty_type_env())];
check([T|Rest]) -> [visit(T, empty_type_env())] ++ check(Rest).

lookup(Name, #chico_type_env{vars=Vars} = _) ->
   lists:search(fun ({N}) -> Name == N end, Vars).

visit({?is_var, Meta, _}, Env) ->
  {_, _, Name} = Meta,

  case lookup(Name, Env) of
    {_} -> ok;
    _ -> throw("Type declaration for " ++ atom_to_list(Name) ++ " was not found")
  end;
visit(_, _Env) -> ok.
