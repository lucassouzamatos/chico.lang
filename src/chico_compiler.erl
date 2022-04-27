-module(chico_compiler).
-export([read_file/1]).

-include("chico.hrl").

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

get_function(N, #chico_parser_env{functions=Functions} = _) ->
  lists:search(fun ({Name, _}) -> Name == N end, Functions).

default_bootstrap(M) -> [
  {attribute,1,module,list_to_atom(M)}, 
  {attribute,1,export,[{start, 0}]}].

construct_form(M, C, #chico_parser_env{exported_functions=ExportedFunctions} = Env) -> 
  Exported = lists:map(
    fun (E) ->
      {value, {N, A}} = get_function(E, Env),
      {attribute,1,export,[{N, A}]} 
    end, 
    ExportedFunctions
  ),
  default_bootstrap(M) ++ Exported ++ C.

hydrate_module_name(N) ->
  string:lowercase(string:replace(N, ".chico", "")).

compile_file(Source, Filename) -> 
  Module = hydrate_module_name(Filename),
  Content = unicode:characters_to_list(Source),

  {ok, Tokens, _} = chico_tokenizer:string(Content),
  {ok, Parsed } = chico_parser:parse(Tokens),
  
  ParserEnv = chico_parser_env:check(Parsed),
  Translated = chico_translate:translate(Parsed, ParserEnv),
  Forms = construct_form(Module, Translated, ParserEnv),

  { ok, Generated, Bin } = compile:forms(Forms),
  
  write_file(Module, Bin),
  run(Generated),

  trace_success_compilation().