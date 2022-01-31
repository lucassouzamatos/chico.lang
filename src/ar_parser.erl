-module(ar_parser).
-export([parse/1]).

-import(ar_utils, [with_rest/2]).
-import(ar_calc, [parse_calc/1]).

parse(Tokens) -> parse_group(Tokens).

parse_group([]) -> [];
parse_group([Token | Rest]) -> 
  {Expr, _Value} = Token,
  if 
    Expr == op ->
      [Result, NewRest] = parse_calc([Token] ++ Rest),
      Result ++ parse_group(NewRest);
    % Expr == variable -> 
    %   [Body, NewRest] =  parse_variable_declaration(Rest), 
    %   [{Expr, Value, Body}] ++ parse_group(NewRest);
    true ->
      [{error, "the program must be initialized with calc", []}]  
  end.

% parse_variable_declaration([Token | Rest]) -> 
%   {Expr, Value} = Token,
%   if Expr == declaration ->
%     [Body, NewRest] = parse_variable_assigment(Rest),
%     with_rest([{Expr, Value, Body}], NewRest);
%   true ->
%     []
%   end.

% parse_variable_assigment([Token | Rest]) ->
%   {Expr, Value} = Token,
%   if Expr == assigment ->
%     [Body, NewRest] = parse_variable_number(Rest),
%     with_rest([{Expr, Value, Body}], NewRest);
%   true ->
%     []
%   end.

% parse_variable_number([Token | Rest]) ->
%   {Expr, Value} = Token,
%   if Expr == integer ->
%     with_rest({Expr, Value, []}, Rest)
%   end.


