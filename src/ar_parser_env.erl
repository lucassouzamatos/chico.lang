-module(ar_parser_env).
-export([check/1]).

-include("chico.hrl").

check([]) -> [];
check([H | Rest]) -> 
  Env = check(H, #ar_parser_env{functions=[]}), 
  check(Rest, Env).

check([H | Rest], Env) -> 
  N = check(H, Env), 
  check(Rest, N);

check({{function, _, _}, {_, _, Name}, _, _}, #ar_parser_env{functions=Functions} = Env) ->
  N = Functions ++ [Name],
  Env#ar_parser_env{functions=N};

check(_, #ar_parser_env{} = Env) ->
  Env.
