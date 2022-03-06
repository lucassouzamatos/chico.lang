-module(ar_compiler).
-export([read_file/1]).

trace(Type, Value) -> io:format("[~p]: ~p~n", [Type, Value]).

trace_file_read_error() -> 
  trace(error, "An error occurred while file read, check if the file exists in the path specified").

trace_success_compilation() -> 
  trace(success, "File compiled successfully").

read_file(F) -> 
  case file:read_file(F) of
    {ok, Source} -> compile_file(Source);
    _ -> trace_file_read_error()
  end.

compile_file(Source) -> 
  Content = unicode:characters_to_list(Source),
  % erlang:display(Content),
  trace_success_compilation().