-module(tokenizer).
-export([tokenize/2, get_token/1]).

% Prevent right hand error when Rest is not defined
tokenize([], _Position) ->
  [];

% Source is scanned words, e.g. ["(", "+", "1", "1", ")"]
tokenize(Source, Position) ->
  [L | Rest] = Source,
  [get_token(L)] ++ tokenize(Rest, Position + 1).

get_token(V) when V == "calc" ->
  match_calc(V);
get_token(V) when V == "\n" ->
  match_breakline(V);
get_token(V) when is_integer(V) ->
  match_integer(V);
get_token(V) when V == "+" ->
  match_plus(V);
get_token(V) when V == "-" -> 
  match_minus(V);
get_token(V) ->
  match_string(V).

match_breakline(_V) -> {breakline, none}.
match_calc(_V) -> {calc, none}.

% Match math operators
match_plus(_V) -> {operator, plus}.
match_minus(_V) -> {operator, minus}.

% Match type numbers
match_integer(V) -> {integer, V}.

% Match string
match_string(V) -> {string, V}.
