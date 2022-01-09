-module(scanner).
-export([scan/0]).

convert_integers(V) ->
  try list_to_integer(V) of
    R -> R
  catch
    _:_ -> V
  end. 

scan() ->
  Source = io:get_line("ar>"),
  Parsed = string:tokens(Source, " "),
  lists:map(fun(T) -> convert_integers(T) end, Parsed).

