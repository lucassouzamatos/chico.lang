-module(main).
-export([execute/0]).
-import(scanner, [scan/1]).

execute() -> 
  Value = io:get_line("ar>"),
  io:format(Value),
  scan(Value),
  ok.
