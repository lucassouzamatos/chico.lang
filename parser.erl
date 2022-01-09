-module(parser).
-export([parse/1]).

parse(Tokens) -> parse_group(Tokens).

parse_group([]) -> [];
parse_group([Token | Rest]) -> 
  {Expr, TokenRest} = Token,
  if 
    Expr == calc ->
      [{Expr, TokenRest, parse_group(Rest)}];
    Expr == operator ->
      [{Expr, TokenRest, parse_operator_group(Rest)}];
    true ->
      []
  end.

parse_operator_group([]) -> [];

parse_operator_group([Token | Rest]) ->
  {Expr, TokenRest} = Token,
  if
    Expr == integer ->
      [{Expr, TokenRest}] ++ parse_operator_group(Rest);
    true -> 
      []
  end.

  
