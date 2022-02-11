-module(ar_compiler).
-export([eval/2]).

trace(V) -> erlang:display(V).

eval(Exprs, Binding) -> 
  {value, Value, NewBinding} = erl_eval:exprs(Exprs, Binding),
  trace(Value),
  NewBinding.

