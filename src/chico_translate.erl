-module(chico_translate).
-export([translate/2]).

-compile({no_auto_import,[apply/2]}).
-compile({no_auto_import,[apply/3]}).

-include("chico.hrl").

spread(A, B) -> 
  list_to_tuple(tuple_to_list(A) ++ tuple_to_list(B)).

operation(Operation, {L, R}) ->
  {Operation, unwrap_hand_side(L), unwrap_hand_side(R)}.

apply(Body) -> 
  case Body of 
    {{operator, Line, Operation}, L, R} -> 
      spread({op, Line}, operation(Operation, {L, R}));
    _ -> error
  end.

apply({declaration, Line, Declaration}, [], Env) ->
  [Dec] = rewrite({declaration, Line, Declaration}, Env),
  {call, Line, Dec, []};

apply({declaration, Line, Declaration}, Args, Env) -> 
  [Dec] = rewrite({declaration, Line, Declaration}, Env),
  {call, Line, Dec, translate(Args, Env)}.

guard(Pattern, Body, E) -> 
  {rewrite(Pattern, E), [], translate(Body, E)}.

clause({GuardPattern, GuardBody}, E) ->
  spread({clause, 1}, guard(GuardPattern, GuardBody, E)).

match(Body, E) -> 
  case Body of
    [{{guard, GuardPattern}, GuardBody} | Rest] -> [clause({GuardPattern, GuardBody}, E)] ++ match(Rest, E);
    _ -> []
  end.

anon_function(Line, Arguments, Body, E) -> 
  {clauses, [{clause, Line, translate(Arguments, E), [], translate(Body, E)}]}.

translate([], _) -> [];
translate([C], #chico_parser_env{} = E) -> rewrite(C, E);
translate([C|Rest], #chico_parser_env{} = Env) -> rewrite(C, Env) ++ translate(Rest, Env).

rewrite({{match, _, _}, Expr, Body}, E) -> 
  [A] = rewrite(Expr, E),
  [{'case', 1, A, match(Body, E)}];

rewrite({apply, Body, Args}, E) -> [apply(Body, Args, E)];
rewrite({apply, Body}, _) -> [apply(Body)];

rewrite({integer, Line, Value}, _) ->
  [{integer, Line, Value}];

rewrite({float, Line, Value}, _) ->
  [{float, Line, Value}];

rewrite({export, _}, _) ->
  [];

rewrite({{variable, Line, _}, {_, _, Name}, {apply, ApplyArgs}}, E) ->
  [Arguments] = rewrite({apply, ApplyArgs}, E),
  [{match, Line, {var, Line, Name}, Arguments}];
rewrite({{variable, Line, _}, {_, _, Name}, {apply, ApplyArgs, ApplyArgs1}}, E) ->
  [Arguments] = rewrite({apply, ApplyArgs, ApplyArgs1}, E),
  [{match, Line, {var, Line, Name}, Arguments}];
rewrite({{variable, Line, _}, {_, _, Name}, {{function, Line, FName}, Arguments, Body}}, E) ->
  [FArguments] = rewrite({{function, Line, FName}, Arguments, Body}, E),
  [{match, Line, {var, Line, Name}, FArguments}];
rewrite({{variable, Line, _}, {_, _, Name}, R}, _) ->
  [{match, Line, {var, Line, Name}, R}];

rewrite({declaration, Line, Name}, Env) -> 
  IsFun = lists:search(
    fun ({N, _}) -> N == Name end, 
    Env#chico_parser_env.functions
  ),
  
  case IsFun of
    false -> [{var, Line, Name}];
    _ -> [{atom, Line, Name}]
  end;

rewrite({{function, Line, _}, Arguments, Body}, E) ->
  [{'fun', Line, anon_function(Line, Arguments, Body, E)}];
rewrite({{function, Line, _}, {_, _, Name}, Arguments, Body}, E) ->
  [{function, Line, Name, 
      length(Arguments), 
      [{clause, Line, translate(Arguments, E), [], translate(Body, E)}]}].

unwrap_hand_side({Type, Line, Value}) when Type == declaration -> 
  {var,Line,Value};
unwrap_hand_side({Type, Line, Value}) ->
  {Type, Line, Value}.
  
