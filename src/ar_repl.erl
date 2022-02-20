-module(ar_repl).
-export([execute/0]).

execute() ->
  do(io:get_line("chico>"), []).

do(Source, _Binding) when Source == "exit\n" -> erlang:display(exited);
do(Source, Binding) ->
  {ok, Tokens, _} = ar_tokenizer:string(Source),
  {ok, Parsed } = ar_parser:parse(Tokens),

  Translated = ar_translate:translate(Parsed),
  NewBinding = ar_evaluate:eval(Translated, Binding),

  do(io:get_line("chico>"), NewBinding).

