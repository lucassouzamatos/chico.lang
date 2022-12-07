-module(chico_parser_env).

-export([check/1]).

-include("chico.hrl").

check([]) -> [];

check([H | Rest]) ->
  Env = check(H, #chico_parser_env{functions = []}),
  check(Rest, Env).


check([H | Rest], Env) ->
  N = check(H, Env),
  check(Rest, N);

check(
  {{function, _, _}, {_, _, Name}, Arguments, _},
  #chico_parser_env{functions = Functions} = Env
) ->
  N = Functions ++ [{Name, length(Arguments)}],
  Env#chico_parser_env{functions = N};

check({export, {_, _, Name}}, #chico_parser_env{exported_functions = EFunctions} = Env) ->
  N = EFunctions ++ [Name],
  Env#chico_parser_env{exported_functions = N};

check(_, #chico_parser_env{} = Env) -> Env.
