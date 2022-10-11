-module(chico_repl).
-export([execute/0]).

execute() ->
  do(io:get_line("chico>"), []).

do(Source, _Binding) when Source == "exit\n" -> erlang:display(exited);
do(Source, Binding) ->
  {ok, Tokens, _} = chico_tokenizer:string(Source),

  case chico_parser:parse(Tokens) of 
    {ok, Parsed } ->
      ParserEnv = chico_parser_env:check(Parsed),

      try chico_type_checker:check(Parsed) of
        _ ->
          Translated = chico_translate:translate(Parsed, ParserEnv),
          NewBinding = eval(Translated, Binding),
          do(io:get_line("chico>"), NewBinding)
      catch Error -> 
        trace(Error),
        do(io:get_line("chico>"), Binding)
      end;

    {error, {{_, Line, _}, _, {internal, Message}}} ->
      trace(Message ++ " at line " ++ integer_to_list(Line)),
      do(io:get_line("chico>"), Binding);
    
    {error, _} ->
      trace("An unknown error happened"),
      do(io:get_line("chico>"), Binding)
  end.

trace(V) -> erlang:display(V).

eval([], Binding) ->
  Binding;

eval(Exprs, Binding) -> 
  {value, Value, NewBinding} = erl_eval:exprs(Exprs, Binding),
  trace(Value),
  NewBinding.

