-module(ar_main).
-export([execute/0]).

execute() ->
  do(io:get_line("ar>"), []).

do(Source, _Binding) when Source == "exit\n" -> erlang:display(exited);
do(Source, Binding) ->
  {ok, Tokens, _} = ar_tokenizer:string(Source),
  {ok, Parsed } = ar_parser:parse(Tokens),

  Translated = ar_translate:translate(Parsed),
  NewBinding = ar_compiler:eval(Translated, Binding),

  do(io:get_line("ar>"), NewBinding).

