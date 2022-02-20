#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa /usr/chicolang/bin -Wall

main([]) ->
  ar_repl:execute(),
  io:format("(\\\/)~n");

% @in progress
main([Option, Value]) when Option == "--compile" ->
  io:format("Start compilation...~p~n", [Value]),
  ar_compiler:read_file(Value);

main([Option]) when Option == "--compile" ->
  io:format("Missing file for compile~n");

main([Option]) ->
  io:format("~p is not a valid command~n", [Option]).
