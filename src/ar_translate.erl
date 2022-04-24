-module(ar_translate).
-export([translate/1]).

-compile({no_auto_import,[apply/2]}).

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

apply(Body, []) -> 
  case Body of 
    {declaration, Line, Declaration} ->
      {call, Line, {var, Line, Declaration}, []}; 
      _ -> error
  end;

apply(Body, Args) -> 
  case Body of 
    {declaration, Line, Declaration} ->
      {call, Line, {var, Line, Declaration}, translate(Args)}; 
    _ -> error
  end.

guard(Pattern, Body) -> 
  {rewrite(Pattern), [], translate(Body)}.

clause({GuardPattern, GuardBody}) ->
  spread({clause, 1}, guard(GuardPattern, GuardBody)).

match(Body) -> 
  case Body of
    [{{guard, GuardPattern}, GuardBody} | Rest] -> [clause({GuardPattern, GuardBody})] ++ match(Rest);
    _ -> []
  end.

translate([]) -> [];
translate([C]) -> rewrite(C);
translate([C|Rest]) -> rewrite(C) ++ translate(Rest).

rewrite({{match, _, _}, Expr, Body}) -> 
  [A] = rewrite(Expr),
  [{'case', 1, A, match(Body)}];

rewrite({apply, Body, Args}) -> [apply(Body, Args)];
rewrite({apply, Body}) -> [apply(Body)];

rewrite({integer, Line, Value}) ->
  [{integer, Line, Value}];

rewrite({float, Line, Value}) ->
  [{float, Line, Value}];

rewrite({{variable, Line, _}, {_, _, Name}, {apply, ApplyArgs}}) ->
  [Arguments] = rewrite({apply, ApplyArgs}),
  [{match, Line, {var, Line, Name}, Arguments}];
rewrite({{variable, Line, _}, {_, _, Name}, {apply, ApplyArgs, ApplyArgs1}}) ->
  [Arguments] = rewrite({apply, ApplyArgs, ApplyArgs1}),
  [{match, Line, {var, Line, Name}, Arguments}];
rewrite({{variable, Line, _}, {_, _, Name}, R}) ->
  [{match, Line, {var, Line, Name}, R}];

rewrite({declaration, Line, Name}) -> 
  [{var, Line, Name}];

rewrite({{function, Line, _}, {_, _, Name}, Arguments, Body}) ->
  [{match, Line,
            {var, Line, Name},
            {'fun',Line,
                   {clauses,[{clause,Line,
                                     translate(Arguments),
                                     [],
                                     translate(Body)}]}}}].


unwrap_hand_side({Type, Line, Value}) when Type == declaration -> 
  {var,Line,Value};
unwrap_hand_side({Type, Line, Value}) ->
  {Type, Line, Value}.
  
