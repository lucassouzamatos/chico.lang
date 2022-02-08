#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -Wall

main(_) ->
  ar_main:execute(),
  io:format("work finished ;) ~n").
