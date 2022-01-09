-module(scanner).
-export([scan/1, convert_integers/1]).

convert_integers(V) ->
  try list_to_integer(V) of
    R -> R
  catch
    _:_ -> V
  end. 

scan(Source) ->
  Parsed = string:tokens(Source, " "),
  lists:map(fun(T) -> convert_integers(T) end, Parsed).

