-module(chico_repl).
-export([execute/0]).

execute() ->
  do(io:get_line("chico>"), []).

do(Source, _Binding) when Source == "exit\n" -> erlang:display(exited);
do(Source, Binding) ->
  {ok, Tokens, _} = chico_tokenizer:string(Source),
  {ok, Parsed } = chico_parser:parse(Tokens),

  Translated = chico_translate:translate(Parsed),
  NewBinding = chico_evaluate:eval(Translated, Binding),

  do(io:get_line("chico>"), NewBinding).

