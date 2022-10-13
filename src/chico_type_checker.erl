-module(chico_type_checker).

-include("chico.hrl").

-export([check/2]).

-define(is_var, {variable, _, _}).
-define(is_integer, {integer, _, _}).
-define(is_string, {string, _, _}).
-define(is_float, {float, _, _}).
-define(is_apply, {apply, _}).
-define(is_operator, {operator, _, _}).

warning_message(Message) -> throw(Message).

check([T], Env) ->
  Infered = infer(T, Env),
  Type = get_type(Infered),
  NewEnv = get_env(Infered),
  {[Type], NewEnv};
check([T|Rest], Env) -> 
  {Result, NewEnv} = check([T], Env),
  {RestResult, RestNewEnv} = check(Rest, NewEnv),
  {Result ++ RestResult, RestNewEnv}.

get_type({Type, _}) -> Type.
get_env({_, Env}) -> Env.

lookup_var(Name, #chico_type_env{vars=Vars} = _) ->
  lists:search(fun ({N, _}) -> Name == N end, Vars).

add_var_to_env(Name, Type, #chico_type_env{vars=Vars} = Env) ->
  N = Vars ++ [{Name, Type}],
  Env#chico_type_env{vars=N}.

anotate(Value) -> {type, Value}.
anotate(Value, Args, Output) -> {type, Value, Args, Output}.

unify({type, Ta}, {type, Tb}) when Ta == Tb -> Ta;
unify({type, Ta}, {type, Tb}) when Ta =/= Tb ->
  warning_message("Type " ++ atom_to_list(Ta) ++ " mismatch to type " ++ atom_to_list(Tb)),
  not_matched_type.

infer({?is_var, {_, _, Name}, Value}, Env) -> 
  Infered = infer(Value, Env),
  Type = get_type(Infered),
  {Type, add_var_to_env(Name, Type, Env)};
infer(?is_integer, Env) -> 
  {anotate(number), Env};
infer(?is_float, Env) -> 
  {anotate(number), Env};
infer(?is_string, Env) -> 
  {anotate(string), Env};
infer({declaration, _, Name}, Env) ->
  {value, {_, Type}} = lookup_var(Name, Env),
  {Type, Env};

%% Infer applications
%% Use constrain (args and result expected) about the function defined and unify with args received
infer({apply, Body}, Env) ->
  [Expression | Args] = tuple_to_list(Body),

  % Expression called
  {constrain, Input, Output} = do_constrain_fn(Expression, Env),
   
  % Arguments type
  TArgs = [get_type(Infered) || Infered <- [infer(Arg, Env) || Arg <- Args]],

  unify_apply_args(Input, TArgs),
  {anotate(call, TArgs, Output), Env};

infer(_, Env) -> {not_found_type, Env}.

get_arg_expected_from_constrain(Index, Constrain) -> lists:nth(Index, Constrain).

%% Receive input constrain with expected args and the args from apply then unify it
unify_apply_args(InputConstrain, TArgs) ->
  [unify(TArg, get_arg_expected_from_constrain(Index, InputConstrain)) || {Index, TArg} <- lists:enumerate(TArgs)].

do_constrain_fn(?is_operator, _Env) ->
  % Constrain with args and result type
  {constrain, [anotate(number), anotate(number)], anotate(number)}.