-module(eval).
-export([evaluate/1]).

evaluate([Token | _Rest]) ->
  {Expr, _Value, TokenRest} = Token,
  if 
    Expr == calc ->
      calculate(TokenRest);
    true ->
      ok
  end.

calculate([Token | _Rest]) ->
  {Expr, Value, TokenRest} = Token,
  if 
    (Expr == operator) and (Value == plus) ->
      plus(TokenRest);
    (Expr == operator) and (Value == minus) ->
      minus(TokenRest);
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


