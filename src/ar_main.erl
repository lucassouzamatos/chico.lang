-module(ar_main).
-export([execute/0]).

execute() ->
  Source = io:get_line("ar>"),
  {ok, Tokens, _} = ar_tokenizer:string(Source),
  {ok, Parsed } = ar_parser:parse(Tokens),
  Translated = ar_translate:translate(Parsed),
  
  ar_compiler:eval(Translated).

