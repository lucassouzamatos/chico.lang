-module(ar_compiler).
-export([eval/1]).

trace(V) -> erlang:display(V).

eval(Exprs) -> 
  {value, Value, _} = erl_eval:exprs(Exprs, []),
  trace(Value).

