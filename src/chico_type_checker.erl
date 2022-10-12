-module(chico_type_checker).

-export([check/1]).

-record(chico_type_env, {
  vars = []
}).

-define(is_var, {variable, _, _}).
-define(is_integer, {integer, _, _}).
-define(is_string, {string, _, _}).
-define(is_float, {float, _, _}).
-define(is_apply, {apply, _}).
-define(is_operator, {operator, _, _}).

empty_type_env() -> #chico_type_env{vars=[]}.

warning_message(Message) -> throw(Message).

check([]) -> [];
check([T]) -> [infer(T, empty_type_env())];
check([T|Rest]) -> [infer(T, empty_type_env())] ++ check(Rest).

lookup(Name, #chico_type_env{vars=Vars} = _) ->
   lists:search(fun ({N}) -> Name == N end, Vars).

anotate(Value) -> {type, Value}.
anotate(Value, Args, Output) -> {type, Value, Args, Output}.

unify({type, Ta}, {type, Tb}) when Ta == Tb -> Ta;
unify({type, Ta}, {type, Tb}) when Ta =/= Tb ->
  warning_message("Type " ++ atom_to_list(Ta) ++ " mismatch to type " ++ atom_to_list(Tb)),
  not_matched_type.

infer({?is_var, _, Value}, Env) -> 
  infer(Value, Env);
infer(?is_integer, _Env) -> 
  anotate(number);
infer(?is_float, _Env) -> 
  anotate(number);
infer(?is_string, _Env) -> 
  anotate(string);

infer({apply, Body}, Env) ->
  [Expression | Args] = tuple_to_list(Body),

  % Expression called
  {constrain, Input, Output} = do_constrain_fn(Expression, Env),
   
  % Arguments type
  TArgs = [infer(Arg, Env) || Arg <- Args],

  [unify(TArg, lists:nth(Index, Input)) || {Index, TArg} <- lists:enumerate(TArgs)],

  anotate(call, TArgs, Output);

infer(_, _Env) -> not_found_type.

do_constrain_fn(?is_operator, _Env) ->
  % Constrain with args and result type
  {constrain, [anotate(number), anotate(number)], anotate(number)}.