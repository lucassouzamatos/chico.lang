-module(ar_main).
-export([execute/0]).

-import(ar_compiler, [eval/1]).

execute() ->
  Source = io:get_line("ar>"),
  {ok, Tokens, _} = ar_tokenizer:string(Source),
  Parsed = ar_parser:parse(Tokens),
  erlang:display(Parsed).
  % Parsed = parse(Tokens),
  % eval(Parsed).
