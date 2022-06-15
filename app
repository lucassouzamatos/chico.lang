#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa /usr/chicolang/bin -Wall

% If not is passed as parameter, the REPL is started
main([]) -> 
  chico_repl:execute(),
  io:format("(\\\/)~n");

main([Option]) when Option == "--install" ->
  chico_bootstrap:start();

%% If --compile option is passed, the file specified is computed
%% Example:
%%   $ chico --compile test.ch
%%   $ [success]: "file compiled successfully"
main([Option, Value]) when Option == "--compile" ->
  chico_compiler:read_file(Value);

main([Option]) when Option == "--compile" ->
  io:format("Missing file for compile~n");

main([Option]) ->
  io:format("~p is not a valid command~n", [Option]).
