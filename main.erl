-module(main).
-export([execute/0]).
-import(scanner, [scan/0]).
-import(tokenizer, [tokenize/2]).
-import(parser, [parse/1]).
-import(erlang, [display/1]).
-import(eval, [evaluate/1]).

execute() -> 
  Source = scan(),
  Tokens = tokenize(Source, 1),
  Parsed = parse(Tokens),
  io:format("~p ~n", [evaluate(Parsed)]),
  ok.
