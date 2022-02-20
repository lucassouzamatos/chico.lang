-module(ar_compiler).
-export([read_file/1]).

read_file(F) -> 
  {ok, File} = file:read_file(F),
  Content = unicode:characters_to_list(File),
  erlang:display(Content).