-module(chico_compiler).
-export([read_file/1]).
-export([compile_file/2]).

-include("chico.hrl").

trace(Type, Value) -> io:format("[~p]: ~p~n", [Type, Value]).

trace_file_read_error() -> 
  trace(error, "An error occurred while file read, check if the file exists in the path specified").

trace_success_compilation() -> 
  trace(success, "File compiled successfully").

read_file(F) -> 
  {ok, Cwd} = file:get_cwd(),
  File = filename:join(Cwd, F),
  erlang:display(file:read_file(File)),
  case file:read_file(File) of
    {ok, Source} -> 
      % try compile_file(Source, File) of 
        { Module, Bin, Generated } = compile_file(Source, File),
          write_file(Module, Bin),
          run(Generated),
          trace_success_compilation();
        % _ -> error
        % catch _:_ -> error
      % end;
    _ -> trace_file_read_error()
  end.

write_file(M, B) -> file:write_file(M ++ ".beam", B).

run(Generated) -> Generated:start().

get_function(N, #chico_parser_env{functions=Functions} = _) ->
  lists:search(fun ({Name, _}) -> Name == N end, Functions).

default_bootstrap(M) -> [{attribute,1,module,list_to_atom(M)}].

construct_form(M, C, #chico_parser_env{exported_functions=ExportedFunctions} = Env) -> 
  Exported = lists:map(
    fun (E) ->
      {value, {N, A}} = get_function(E, Env),
      {attribute,1,export,[{N, A}]} 
    end, 
    ExportedFunctions
  ),
  default_bootstrap(M) ++ Exported ++ C.

compile_file(Source, Filename) -> 
  Module = filename:basename(Filename, ".chico"),
  Content = unicode:characters_to_list(Source),

  {ok, Tokens, _} = chico_tokenizer:string(Content),
  erlang:display(Tokens),

  case chico_parser:parse(Tokens) of 
    {ok, Parsed } ->
      erlang:display(Parsed),

      ParserEnv = chico_parser_env:check(Parsed),
      Translated = chico_translate:translate(Parsed, ParserEnv),
      erlang:display(Translated),

      Forms = construct_form(Module, Translated, ParserEnv),

      { ok, Generated, Bin } = compile:forms(Forms),

      { Module, Bin, Generated };

    {error, {{_, Line, _}, _, {internal, Message}}} ->
      trace(error, Message ++ " at line "  ++ integer_to_list(Line));
    
    {error, E} ->
      trace(error, E),
      trace(error, "An unknown error happened")
  end.