-module(parser).
-export([parse/1]).

parse(Tokens) -> parse_group(Tokens).

parse_group([]) -> [];
parse_group([Token | Rest]) -> 
  {Expr, Body} = Token,
  if 
    Expr == calc ->
      [{Expr, Body, parse_group(Rest)}];
    Expr == operator ->
      [{Expr, Body, parse_operator_group(Rest)}];
    true ->
      []
  end.

parse_operator_group([]) -> [];
parse_operator_group([Token | Rest]) ->
  {Expr, Body} = Token,
  if
    Expr == integer ->
      [{Expr, Body}] ++ parse_operator_group(Rest);
    true -> 
      []
  end.

  
