-module(chico_codegen_test).

-include_lib("eunit/include/eunit.hrl").

get_tokens(Value) ->
  {ok, Tokens, _} = chico_tokenizer:string(Value),
  Tokens.


parse(Value) ->
  {ok, Parsed} = chico_parser:parse(get_tokens(Value)),
  Parsed.


simple_operator_test() ->
  V = "apply + 55 52 done",
  P = parse(V),
  Env = chico_parser_env:check(P),
  [{op, 1, '+', {integer, 1, 55}, {integer, 1, 52}}] = chico_codegen:translate(P, Env).


call_function_test() ->
  V = "apply sum 1 done",
  P = parse(V),
  Env = chico_parser_env:check(P),
  [{call, 1, {var, 1, sum}, [{integer, 1, 1}]}] = chico_codegen:translate(P, Env).


call_function_with_list_test() ->
  V = "apply sum [] done",
  P = parse(V),
  Env = chico_parser_env:check(P),
  erlang:display(chico_codegen:translate(P, Env)).
