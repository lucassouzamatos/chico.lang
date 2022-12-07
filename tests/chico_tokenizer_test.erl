-module(chico_tokenizer_test).

-include_lib("eunit/include/eunit.hrl").

operator_test() ->
  {ok, [{integer, 1, 55}, {operator, 1, '+'}, {integer, 1, 52}], _} =
    chico_tokenizer:string("55 + 52"),
  {ok, [{integer, 1, 55}, {operator, 1, '*'}, {declaration, 1, 'ABC'}], _} =
    chico_tokenizer:string("55 * ABC").
