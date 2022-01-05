-module(scanner).
-export([scan/1]).

print_tokens(List) ->
  lists:foreach(fun({Type, Line, Value}) -> io:format("~p: ~p ~p~n", [Line, Type, Value]) end, List).

normalize_tokens(Words, Line) ->
  lists:map(fun(T) -> {token, Line, T} end, Words).

normalize_tokens(Words) ->
  normalize_tokens(Words, 1).

scan(Source) ->
  Words = re:split(Source, " "),
  Tokens = normalize_tokens(Words),
  print_tokens(Tokens).

