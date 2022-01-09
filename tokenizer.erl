-module(tokenizer).
-export([tokenize/2, get_token/1]).

% Prevent right hand error when Rest is not defined
tokenize([], _Position) ->
  [];

% Source is scanned words, e.g. ["(", "+", "1", "1", ")"]
tokenize(Source, Position) ->
  [L | Rest] = Source,
  [get_token(L)] ++ tokenize(Rest, Position + 1).

get_token(Value) ->
  if 
    Value == "\n" ->
       {breakline, none};
    Value == "calc" ->
      {calc, none};
    Value == "+" ->
      {operator, plus};
    Value == "-" ->
      {operator, minus};
    is_integer(Value) ->
      {integer, Value};
    true ->
      {string, Value}
  end.
