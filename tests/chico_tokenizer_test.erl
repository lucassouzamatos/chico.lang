-module(chico_tokenizer_test).

-include_lib("eunit/include/eunit.hrl").

operators_test() ->
  {ok, [{integer, 1, 55}, {operator, 1, '+'}, {integer, 1, 52}], _} =
    chico_tokenizer:string("55 + 52").
