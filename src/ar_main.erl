-module(ar_main).
-export([execute/0]).
-import(ar_scanner, [scan/0]).
-import(ar_tokenizer, [tokenize/2]).
-import(ar_parser, [parse/1]).
-import(ar_compiler, [eval/1]).

execute() -> 
  Source = scan(),
  Tokens = tokenize(Source, 1),
  Parsed = parse(Tokens),
  eval(Parsed).
