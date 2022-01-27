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
    Expr == variable -> 
      [VariableDeclaration, VariableDeclarationRest] =  parse_variable_declaration(Rest), 
      [{Expr, Value, VariableDeclaration}] ++ parse_group(VariableDeclarationRest);
    true ->
      [{error, "the program must be initialized with calc", []}]  
  end.

with_rest(Value, Rest) ->
  [Value, Rest].

parse_variable_declaration([Token | Rest]) -> 
  {Expr, Value} = Token,
  if Expr == declaration ->
    [VariableAssigment, VariableAssigmentRest] = parse_variable_assigment(Rest),
    with_rest([{Expr, Value, VariableAssigment}], VariableAssigmentRest);
  true ->
    []
  end.

parse_variable_assigment([Token | Rest]) ->
  {Expr, Value} = Token,
  if Expr == assigment ->
    [VariableNumber, VariableNumberRest] = parse_variable_number(Rest),
    with_rest([{Expr, Value, VariableNumber}], VariableNumberRest);
  true ->
    []
  end.

parse_variable_number([Token | Rest]) ->
  {Expr, Value} = Token,
  if Expr == integer ->
    with_rest({Expr, Value, []}, Rest)
  end.

% Calculation parsers
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
      [{error, "after operator must be declared the value", []}]
  end.

  
