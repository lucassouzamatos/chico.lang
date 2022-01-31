-module(ar_parser).
-export([parse/1]).

-import(ar_calc, [parse_calc/1]).

parse([]) -> [];

parse([{Expr, Value} | Rest]) when Expr == op ->
  [Result, NewRest] = parse_calc([{Expr, Value}] ++ Rest),
  Result ++ parse(NewRest);

parse(_) ->
  [{error, "an error occurred"}].

