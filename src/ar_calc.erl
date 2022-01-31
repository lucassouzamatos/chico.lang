-module(ar_calc).
-export([parse_calc/1]).

-import(ar_utils, [with_rest/2]).

parse_calc([]) -> [];
parse_calc([{Expr, _Value} | Rest]) ->
  [Op, NewRest] = parse_operator(Rest),

  {_, OpLine, OpValue, OpBody} = Op,
  [Left, Right] = OpBody,

  with_rest([{Expr, OpLine, OpValue, Left, Right}], NewRest).

parse_operator([]) -> [];

parse_operator([{Expr, Value} | Rest]) when Expr == operator ->
  [Body, NewRest] =  parse_left(Rest),
  with_rest({Expr, 1, Value, Body}, NewRest);

parse_operator(_) ->
  [{error, "after calc must be declared the operator", []}].


parse_left([]) -> [];

parse_left([{Expr, Value} | Rest]) when Expr == integer ->
  [Body, NewRest] = parse_right(Rest),
  with_rest([{Expr, 1, Value}] ++ Body, NewRest);

parse_left(_) ->
  [{error, "after operator must be declared the value", []}].


parse_right([{Expr, Value} | Rest]) when Expr == integer ->
  with_rest([{Expr, 1, Value}], Rest);

parse_right(_) ->
   [{error, "after operator must be declared the value", []}].

