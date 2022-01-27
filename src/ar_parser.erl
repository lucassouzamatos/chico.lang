-module(ar_parser).
-export([parse/1]).

parse(Tokens) -> parse_group(Tokens).

parse_group([]) -> [];
parse_group([Token | Rest]) -> 
  {Expr, Value} = Token,
  if 
    Expr == calc ->
      [{Expr, Value, parse_calculate_group(Rest)}];
    true ->
      [{error, "the program must be initialized with calc", []}]
  end.

parse_calculate_group([]) -> [];
parse_calculate_group([Token | Rest]) ->
  {Expr, Value} = Token,
  if 
    Expr == operator -> 
      [{Expr, Value, parse_operator_group(Rest)}];
    true ->
      [{error, "after calc must be declared the operator", []}]
  end.

parse_operator_group([]) -> [];
parse_operator_group([Token | Rest]) ->
  {Expr, Value} = Token,
  if
    Expr == integer ->
      [{Expr, Value}] ++ parse_operator_group(Rest);
    % Todo: remove breakline from AST
    Expr == breakline -> 
      [];
    true -> 
      [{error, "after operator must be declared the value", []}]
  end.

  
