-module(ar_calc).
-export([parse_calc/1]).

-import(ar_utils, [with_rest/2]).

parse_calc([]) -> [];
parse_calc([Token | Rest]) ->
  {Expr, _Value} = Token,
  [Op, NewRest] = parse_operator(Rest),

  {_, OpLine, OpValue, OpBody} = Op,
  [Left, Right] = OpBody,

  with_rest([{Expr, OpLine, OpValue, Left, Right}], NewRest).

parse_operator([]) -> [];
parse_operator([Token | Rest]) ->
  {Expr, Value} = Token,
  if 
    Expr == operator ->
      [Body, NewRest] =  parse_left(Rest),
      with_rest({Expr, 1, Value, Body}, NewRest);
    true ->
      [{error, "after calc must be declared the operator", []}]
  end.

parse_left([]) -> [];
parse_left([Token | Rest]) ->
  {Expr, Value} = Token,
  if
    Expr == integer ->
      [Body, NewRest] = parse_right(Rest), 
      with_rest([{Expr, 1, Value}] ++ Body, NewRest);
    true -> 
      with_rest([], Rest)
  end.

parse_right([Token | Rest]) ->
  {Expr, Value} = Token,
  if
    Expr == integer -> 
      with_rest([{Expr, 1, Value}], Rest);
    true -> 
      [{error, "after operator must be declared the value", []}]
  end.

