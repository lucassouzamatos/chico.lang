-module(eval).
-export([evaluate/1]).

evaluate([Token | _Rest]) ->
  {Expr, _Value, Body} = Token,
  if 
    Expr == calc ->
      calculate(Body);
    true ->
      ok
  end.

calculate([Token | _Rest]) ->
  {Expr, Value, Body} = Token,
  if 
    (Expr == operator) and (Value == plus) ->
      plus(Body);
    (Expr == operator) and (Value == minus) ->
      minus(Body);
    true ->
      ok
  end.

plus([]) -> 0;
plus([Token | Rest]) ->
  {Expr, Value} = Token,
  if 
    Expr == integer ->
      Value + plus(Rest);
    true ->
      Value
  end.

minus([]) -> 0;
minus([Token | Rest]) ->
  {Expr, Value} = Token,
  if 
    Expr == integer ->
      Value - plus(Rest);
    true ->
      Value
  end.


