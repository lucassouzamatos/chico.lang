-module(chico_type_checker).

-export([check/1]).

-record(chico_type_env, {
  vars = []
}).

-define(is_var, {variable, _, _}).

empty_type_env() -> #chico_type_env{vars=[]}.

warning_message(Message) -> erlang:display(Message).

check([]) -> [];
check([T]) -> [infer(T, empty_type_env())];
check([T|Rest]) -> [infer(T, empty_type_env())] ++ check(Rest).

lookup(Name, #chico_type_env{vars=Vars} = _) ->
   lists:search(fun ({N}) -> Name == N end, Vars).

infer({?is_var, Meta, _}, Env) ->
  {_, _, Name} = Meta,

  case lookup(Name, Env) of
    {_} -> ok;
    _ -> warning_message("Type declaration for " ++ atom_to_list(Name) ++ " was not found")
  end;
infer(_, _Env) -> ok.

