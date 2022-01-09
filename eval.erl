-module(eval).
-export([evaluate/1]).

evaluate([Token | _Rest]) ->
  {Expr, _Value, Body} = Token,
  if 
    Expr == calc ->
      calculate(Body);
    true ->
      trace_error(Token),
      exit
  end.

trace_error(Token) -> 
  {_Expr, Value, _Body} = Token,
  io:format("error: ~p ~n", [Value]). 

calculate([Token | _Rest]) ->
  {Expr, Value, Body} = Token,
  if 
    Expr == operator ->
      operate(Body, Value);
    (Expr == error) ->
      trace_error(Token);
    true ->
      ok
  end.

operate([], Operator) ->
  if 
    (Operator == plus) or (Operator == minus) ->
       0;
     true -> 0
  end;
operate([Token | Rest], Operator) ->
  {Expr, Value} = Token,
  if 
    (Expr == integer) and (Operator == plus) ->
      Value + operate(Rest, Operator);
    (Expr == integer) and (Operator == minus) ->
      Value - operate(Rest, Operator);
    Expr == error ->
      trace_error(Token);
    true ->
      0
  end.

