-module(chico_repl).
-export([execute/0]).

execute() ->
  do(io:get_line("chico>"), []).

do(Source, _Binding) when Source == "exit\n" -> erlang:display(exited);
do(Source, Binding) ->
  {ok, Tokens, _} = chico_tokenizer:string(Source),
  {ok, Parsed } = chico_parser:parse(Tokens),

  ParserEnv = chico_parser_env:check(Parsed),

  Translated = chico_translate:translate(Parsed, ParserEnv),
  NewBinding = eval(Translated, Binding),

  do(io:get_line("chico>"), NewBinding).

trace(V) -> erlang:display(V).

eval(Exprs, Binding) -> 
  {value, Value, NewBinding} = erl_eval:exprs(Exprs, Binding),
  trace(Value),
  NewBinding.

