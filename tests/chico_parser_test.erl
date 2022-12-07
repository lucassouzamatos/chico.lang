-module(chico_parser_test).

-include_lib("eunit/include/eunit.hrl").

get_tokens(Value) ->
  {ok, Tokens, _} = chico_tokenizer:string(Value),
  Tokens.


simple_operator_test() ->
  {ok, [{apply, {{operator, 1, '+'}, {integer, 1, 55}, {integer, 1, 52}}}]} =
    chico_parser:parse(get_tokens("apply + 55 52 done")).

call_function_test() ->
  {ok, [{apply, {declaration, 1, sum}, [{integer, 1, 1}]}]} =
    chico_parser:parse(get_tokens("apply sum 1 done")).

call_function_with_list_test() ->
  {ok, [{apply, {declaration, 1, sum}, [{list, []}]}]} =
    chico_parser:parse(get_tokens("apply sum [] done")).

call_function_with_tuple_test() ->
  {ok, [{apply, {declaration, 1, sum}, [{tuple, [{integer, 1, 1}, {integer, 1, 2}]}]}]} =
    chico_parser:parse(get_tokens("apply sum {1 2} done")).
