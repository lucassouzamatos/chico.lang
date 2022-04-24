-module(ar_compiler).
-export([read_file/1]).

trace(Type, Value) -> io:format("[~p]: ~p~n", [Type, Value]).

trace_file_read_error() -> 
  trace(error, "An error occurred while file read, check if the file exists in the path specified").

trace_success_compilation() -> 
  trace(success, "File compiled successfully").

read_file(F) -> 
  case file:read_file(F) of
    {ok, Source} -> compile_file(Source, F);
    _ -> trace_file_read_error()
  end.

write_file(M, B) -> file:write_file(M ++ ".beam", B).

run(Generated) -> trace(result, Generated:start()).

construct_form(M, C) -> 
  [{attribute,1,module,list_to_atom(M)},
   {attribute,1,export,[{start,0}]},
   {function,1,start,0,
             [{clause,1,
                     [],
                     [],
                     C}]}].

hydrate_module_name(N) ->
  string:lowercase(string:replace(N, ".ch", "")).

compile_file(Source, Filename) -> 
  Module = hydrate_module_name(Filename),
  Content = unicode:characters_to_list(Source),

  {ok, Tokens, _} = ar_tokenizer:string(Content),
  {ok, Parsed } = ar_parser:parse(Tokens),
  Translated = ar_translate:translate(Parsed),

  Forms = construct_form(Module, Translated),

  { ok, Generated, Bin } = compile:forms(Forms),

  write_file(Module, Bin),
  run(Generated),

  trace_success_compilation().