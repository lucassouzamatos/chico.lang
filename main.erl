-module(main).
-export([execute/0]).
-import(scanner, [scan/1]).
-import(tokenizer, [tokenize/2]).
-import(parser, [parse/1]).
-import(erlang, [display/1]).

print_token({Expr, Value}) -> 
  io:format("~p, ~p~n", [Expr, Value]);
print_token({Expr, Value, Rest}) ->
  print_token({Expr, Value}),
  print_tokens(Rest).

print_tokens(List) ->
  lists:foreach(fun (Value) -> print_token(Value) end, List).

execute() -> 
  Value = io:get_line("ar>"),
  Source = scan(Value),
  Tokens = tokenize(Source, 1),
  Parsed = parse(Tokens),
  print_tokens(Parsed),
  ok.
