-module(ar_calc).
-export([parse_calculate_group/1]).

-import(ar_utils, [with_rest/2]).

parse_calculate_group([]) -> [];
parse_calculate_group([Token | Rest]) ->
  {Expr, Value} = Token,
  if 
    Expr == operator ->
      [Body, NewRest] =  parse_operator_group(Rest),
      with_rest([{Expr, Value, Body}], NewRest);
    true ->
      [{error, "after calc must be declared the operator", []}]
  end.

parse_operator_group([]) -> [];
parse_operator_group([Token | Rest]) ->
  {Expr, Value} = Token,
  if
    Expr == integer ->
      [Body, NewRest] = parse_operator_group(Rest, true), 
      with_rest([{Expr, Value}] ++ Body, NewRest);
    true -> 
      with_rest([], Rest)
  end.

parse_operator_group([Token | Rest], _Done) ->
{Expr, Value} = Token,
  if
    Expr == integer -> 
      with_rest([{Expr, Value}], Rest);
    true -> 
      [{error, "after operator must be declared the value", []}]
  end.

