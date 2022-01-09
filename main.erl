-module(main).
-export([execute/0]).
-import(scanner, [scan/1]).
-import(tokenizer, [tokenize/2]).

print_token({Type, Value}) -> io:format("~p, ~p~n", [Type, Value]).

print_tokens(List) ->
  lists:foreach(fun (Value) -> print_token(Value) end, List).

execute() -> 
  Value = io:get_line("ar>"),
  Source = scan(Value),
  Tokens = tokenize(Source, 1),
  print_tokens(Tokens),
  ok.
