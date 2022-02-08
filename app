#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa /usr/arlang/bin -Wall

main(_) ->
  ar_main:execute(),
  io:format("work finished ;) ~n").
