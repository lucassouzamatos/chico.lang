-module(ar_env).
-export([new/0, insert_var/2]).

new() ->
  ets:new(env, [ordered_set]).

insert_var(Env, Value) ->
  ets:insert(Env, {"@var-" ++ Value}).
