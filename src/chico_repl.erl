-module(chico_repl).
-export([execute/0]).

execute() ->
  do(io:get_line("chico>"), [], chico_type_env:empty_type_env()).

do(Source, _Binding, _TypingEnv) when Source == "exit\n" -> erlang:display(exited);
do(Source, Binding, TypingEnv) ->
  {ok, Tokens, _} = chico_tokenizer:string(Source),
  case chico_parser:parse(Tokens) of 
    {ok, Parsed } ->
      ParserEnv = chico_parser_env:check(Parsed),

      try chico_type_checker:check(Parsed, TypingEnv) of
        {_TypingAST, NewTypingEnv} ->
          Translated = chico_codegen:translate(Parsed, ParserEnv),
          NewBinding = eval(Translated, Binding),
          do(io:get_line("chico>"), NewBinding, NewTypingEnv)
      catch Error -> 
        trace(Error),
        do(io:get_line("chico>"), Binding, TypingEnv)
      end;

    {error, {{_, Line, _}, _, {internal, Message}}} ->
      trace(Message ++ " at line " ++ integer_to_list(Line)),
      do(io:get_line("chico>"), Binding, TypingEnv);
    
    {error, _} ->
      trace("An unknown error happened"),
      do(io:get_line("chico>"), Binding, TypingEnv)
  end.

trace(V) -> erlang:display(V).

eval([], Binding) ->
  Binding;

eval(Exprs, Binding) -> 
  {value, Value, NewBinding} = erl_eval:exprs(Exprs, Binding),
  trace(Value),
  NewBinding.


