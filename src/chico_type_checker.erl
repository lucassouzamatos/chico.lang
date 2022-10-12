-module(chico_type_checker).

-export([check/1]).

-record(chico_type_env, {
  vars = []
}).

-define(is_var, {variable, _, _}).
-define(is_integer, {integer, _, _}).
-define(is_float, {float, _, _}).
-define(is_apply, {apply, _}).
-define(is_operator, {operator, _, _}).

empty_type_env() -> #chico_type_env{vars=[]}.

warning_message(Message) -> erlang:display(Message).

check([]) -> [];
check([T]) -> [infer(T, empty_type_env())];
check([T|Rest]) -> [infer(T, empty_type_env())] ++ check(Rest).

lookup(Name, #chico_type_env{vars=Vars} = _) ->
   lists:search(fun ({N}) -> Name == N end, Vars).

anotate(Value) -> {type, Value}.

infer({?is_var, _, Value}, Env) -> 
  infer(Value, Env);
infer(?is_integer, _Env) -> 
  anotate(number);
infer(?is_float, _Env) -> 
  anotate(number);
infer(?is_operator, _Env) ->
  % Constrain with args and result type
  {constrain, [anotate(number), anotate(number)], anotate(number)};
infer({apply, Body}, Env) ->
  [Expression | Args] = tuple_to_list(Body),

  % Expression called
  Ec = infer(Expression, Env),
   
  % Arguments type
  At = [infer(Arg, Env) || Arg <- Args];
infer(_, _Env) -> ok.

