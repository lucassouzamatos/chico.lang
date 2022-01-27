-module(ar_parser).
-export([parse/1]).

parse(Tokens) -> parse_group(Tokens).

parse_group([]) -> [];
parse_group([Token | Rest]) -> 
  {Expr, Value} = Token,
  if 
    Expr == calc ->
      [Body, NewRest] =  parse_calculate_group(Rest), 
      [{Expr, Value, Body}] ++ parse_group(NewRest);
    Expr == variable -> 
      [Body, NewRest] =  parse_variable_declaration(Rest), 
      [{Expr, Value, Body}] ++ parse_group(NewRest);
    true ->
      [{error, "the program must be initialized with calc", []}]  
  end.

with_rest(Value, Rest) ->
  [Value, Rest].

parse_variable_declaration([Token | Rest]) -> 
  {Expr, Value} = Token,
  if Expr == declaration ->
    [Body, NewRest] = parse_variable_assigment(Rest),
    with_rest([{Expr, Value, Body}], NewRest);
  true ->
    []
  end.

parse_variable_assigment([Token | Rest]) ->
  {Expr, Value} = Token,
  if Expr == assigment ->
    [Body, NewRest] = parse_variable_number(Rest),
    with_rest([{Expr, Value, Body}], NewRest);
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

