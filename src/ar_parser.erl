-module(ar_parser).
-export([parse/1]).

parse(Tokens) -> parse_group(Tokens).

parse_group([]) -> [];
parse_group([Token | Rest]) -> 
  {Expr, Value} = Token,
  if 
    Expr == calc ->
      [ValueGroupCalculate, ValueGroupCalculateRest] =  parse_calculate_group(Rest), 
      [{Expr, Value, ValueGroupCalculate}] ++ parse_group(ValueGroupCalculateRest);
    true ->
      nil  
  end.

with_rest(Value, Rest) ->
  [Value, Rest].

parse_calculate_group([]) -> [];
parse_calculate_group([Token | Rest]) ->
  {Expr, Value} = Token,
  if 
    Expr == operator ->
      [ValueGroupOperator, ValueGroupOperatorRest] =  parse_operator_group(Rest),
      with_rest([{Expr, Value, ValueGroupOperator}], ValueGroupOperatorRest);
    true ->
      [{error, "after calc must be declared the operator", []}]
  end.

parse_operator_group([]) -> [];
parse_operator_group([Token | Rest]) ->
  {Expr, Value} = Token,
  if
    Expr == integer ->
      [ValueGroupOperator, ValueGroupOperatorRest] = parse_operator_group(Rest, true), 
      with_rest([{Expr, Value}] ++ ValueGroupOperator, ValueGroupOperatorRest);
    true -> 
      with_rest([], Rest)
  end.

parse_operator_group([Token | Rest], _Done) ->
{Expr, Value} = Token,
  if
    Expr == integer -> 
      with_rest([{Expr, Value}], Rest);
    true -> 
      with_rest([], Rest)
  end.

  
