-module(ar_utils).
-export([with_rest/2]).

with_rest(Value, Rest) ->
  [Value, Rest].

