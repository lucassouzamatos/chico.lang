-module(chico_parser).

-export([parse/1, parse_and_scan/1, format_error/1]).

-file("chico_parser.yrl", 160).
-file("/opt/homebrew/Cellar/erlang/25.2/lib/erlang/lib/parsetools-2.4.1/include/yeccpre.hrl", 0).

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% The parser generator will insert appropriate declarations before this line.%
-type yecc_ret() :: {error, _}
                  | {ok, _}.

-spec parse(Tokens :: list()) -> yecc_ret().
parse(Tokens) -> yeccpars0(Tokens, {no_func, no_location}, 0, [], []).

-spec parse_and_scan({function() | {atom(), atom()}, [_]} | {atom(), atom(), [_]}) -> yecc_ret().
parse_and_scan({F, A}) -> yeccpars0([], {{F, A}, no_location}, 0, [], []);

parse_and_scan({M, F, A}) ->
  Arity = length(A),
  yeccpars0([], {{fun M:F/Arity, A}, no_location}, 0, [], []).


-spec format_error(any()) -> [char() | list()].
format_error(Message) ->
  case io_lib:deep_char_list(Message) of
    true -> Message;
    _ -> io_lib:write(Message)
  end.

%% To be used in grammar files to throw an error message to the parser
%% toplevel. Doesn't have to be exported!

-compile({nowarn_unused_function, return_error/2}).

-spec return_error(erl_anno:location(), any()) -> no_return().
return_error(Location, Message) -> throw({error, {Location, ?MODULE, Message}}).

-define(CODE_VERSION, "1.4").

yeccpars0(Tokens, Tzr, State, States, Vstack) -> try yeccpars1(Tokens, Tzr, State, States, Vstack) catch
    error : Error:Stacktrace ->
      try yecc_error_type(Error, Stacktrace) of
        Desc -> erlang:raise(error, {yecc_bug, ?CODE_VERSION, Desc}, Stacktrace)
      catch
        _:_ -> erlang:raise(error, Error, Stacktrace)
      end;

    %% Probably thrown from return_error/2:
    throw : {error, {_Location, ?MODULE, _M}} = Error -> Error end.

yecc_error_type(function_clause, [{?MODULE, F, ArityOrArgs, _} | _]) ->
  case atom_to_list(F) of
    "yeccgoto_" ++ SymbolL ->
      {ok, [{atom, _, Symbol}], _} = erl_scan:string(SymbolL),
      State =
        case ArityOrArgs of
          [S, _, _, _, _, _, _] -> S;
          _ -> state_is_unknown
        end,
      {Symbol, State, missing_in_goto_table}
  end.


yeccpars1([Token | Tokens], Tzr, State, States, Vstack) ->
  yeccpars2(State, element(1, Token), States, Vstack, Token, Tokens, Tzr);

yeccpars1([], {{F, A}, _Location}, State, States, Vstack) ->
  case apply(F, A) of
    {ok, Tokens, EndLocation} -> yeccpars1(Tokens, {{F, A}, EndLocation}, State, States, Vstack);
    {eof, EndLocation} -> yeccpars1([], {no_func, EndLocation}, State, States, Vstack);
    {error, Descriptor, _EndLocation} -> {error, Descriptor}
  end;

yeccpars1([], {no_func, no_location}, State, States, Vstack) ->
  Line = 999999,
  yeccpars2(State, '$end', States, Vstack, yecc_end(Line), [], {no_func, Line});

yeccpars1([], {no_func, EndLocation}, State, States, Vstack) ->
  yeccpars2(State, '$end', States, Vstack, yecc_end(EndLocation), [], {no_func, EndLocation}).

%% yeccpars1/7 is called from generated code.
%%
%% When using the {includefile, Includefile} option, make sure that
%% yeccpars1/7 can be found by parsing the file without following
%% include directives. yecc will otherwise assume that an old
%% yeccpre.hrl is included (one which defines yeccpars1/5).

yeccpars1(State1, State, States, Vstack, Token0, [Token | Tokens], Tzr) ->
  yeccpars2(State, element(1, Token), [State1 | States], [Token0 | Vstack], Token, Tokens, Tzr);

yeccpars1(State1, State, States, Vstack, Token0, [], {{_F, _A}, _Location} = Tzr) ->
  yeccpars1([], Tzr, State, [State1 | States], [Token0 | Vstack]);

yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, no_location}) ->
  Location = yecctoken_end_location(Token0),
  yeccpars2(
    State,
    '$end',
    [State1 | States],
    [Token0 | Vstack],
    yecc_end(Location),
    [],
    {no_func, Location}
  );

yeccpars1(State1, State, States, Vstack, Token0, [], {no_func, Location}) ->
  yeccpars2(
    State,
    '$end',
    [State1 | States],
    [Token0 | Vstack],
    yecc_end(Location),
    [],
    {no_func, Location}
  ).

%% For internal use only.

yecc_end(Location) -> {'$end', Location}.

yecctoken_end_location(Token) ->
  try erl_anno:end_location(element(2, Token)) of
    undefined -> yecctoken_location(Token);
    Loc -> Loc
  catch
    _:_ -> yecctoken_location(Token)
  end.

-compile({nowarn_unused_function, yeccerror/1}).

yeccerror(Token) ->
  Text = yecctoken_to_string(Token),
  Location = yecctoken_location(Token),
  {error, {Location, ?MODULE, ["syntax error before: ", Text]}}.

-compile({nowarn_unused_function, yecctoken_to_string/1}).

yecctoken_to_string(Token) ->
  try erl_scan:text(Token) of
    undefined -> yecctoken2string(Token);
    Txt -> Txt
  catch
    _:_ -> yecctoken2string(Token)
  end.


yecctoken_location(Token) -> try erl_scan:location(Token) catch _:_ -> element(2, Token) end.

-compile({nowarn_unused_function, yecctoken2string/1}).

yecctoken2string(Token) ->
  try yecctoken2string1(Token) catch _:_ -> io_lib:format("~tp", [Token]) end.

-compile({nowarn_unused_function, yecctoken2string1/1}).

yecctoken2string1({atom, _, A}) -> io_lib:write_atom(A);
yecctoken2string1({integer, _, N}) -> io_lib:write(N);
yecctoken2string1({float, _, F}) -> io_lib:write(F);
yecctoken2string1({char, _, C}) -> io_lib:write_char(C);
yecctoken2string1({var, _, V}) -> io_lib:format("~s", [V]);
yecctoken2string1({string, _, S}) -> io_lib:write_string(S);
yecctoken2string1({reserved_symbol, _, A}) -> io_lib:write(A);
yecctoken2string1({_Cat, _, Val}) -> io_lib:format("~tp", [Val]);
yecctoken2string1({dot, _}) -> "'.'";
yecctoken2string1({'$end', _}) -> [];
yecctoken2string1({Other, _}) when is_atom(Other) -> io_lib:write_atom(Other);
yecctoken2string1(Other) -> io_lib:format("~tp", [Other]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-file("chico_parser.erl", 184).

-dialyzer({nowarn_function, yeccpars2/7}).

-compile({nowarn_unused_function, yeccpars2/7}).

yeccpars2(0 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(1=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_1(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(2=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_2(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(3=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_3(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(4=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_4(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(5=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_5(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(6=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_6(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(7=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_7(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(8=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_8(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(9=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_9(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(10=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_10(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(11=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_11(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(12=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_12(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(13=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_13(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(14=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_14(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(15=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_15(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(16 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(17 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_17(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(18 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_18(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(19 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_19(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(20 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_20(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(21 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_21(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(22 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_22(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(23 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_23(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(24 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_24(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(25 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_25(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(26 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_26(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(27 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_27(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(28 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(29 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_29(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(30 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_30(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(31=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_31(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(32=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_32(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(33=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_33(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(34=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_34(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(35=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_35(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(36 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_36(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(37=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_37(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(38 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_38(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(39 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_39(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(40 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(41 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_41(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(42 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_42(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(43=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_43(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(44=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_44(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(45=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_45(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(46=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_46(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(47=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_47(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(48=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_48(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(49 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_49(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(50 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_50(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(51=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_51(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(52=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_52(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(53 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(54=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_54(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(55 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_55(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(56 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_56(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(57 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(58=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_58(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(59 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_59(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(60 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_60(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(61 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_61(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(62 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(63=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_63(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(64=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_64(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(65 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_65(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(66 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_66(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(67 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_67(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(68=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_68(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(69 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_69(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(70=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_70(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(71 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_71(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(72 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(73=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_73(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(74 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_74(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(75 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_75(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(76=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_76(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(77 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_77(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(78 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(79=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_79(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(80 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_80(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(81 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_81(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(82 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(83=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_83(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(84 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_84(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(85 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_85(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(86 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_86(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(87=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_87(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(88=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_88(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(89 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(90 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(91 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_91(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(92=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_92(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(93 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_93(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(94 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_94(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(95 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_95(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(96=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_96(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(97 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_97(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(98=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(99=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_99(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(100=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_100(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(101 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_101(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(102 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_102(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(103 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_103(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(104 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_104(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(105=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_105(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(106 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_106(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(107 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_107(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(108=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_108(S, Cat, Ss, Stack, T, Ts, Tzr);
%% yeccpars2(109=S, Cat, Ss, Stack, T, Ts, Tzr) ->
%%  yeccpars2_109(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(110 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_110(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(111 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_111(S, Cat, Ss, Stack, T, Ts, Tzr);
yeccpars2(112 = S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_112(S, Cat, Ss, Stack, T, Ts, Tzr);

yeccpars2(Other, _, _, _, _, _, _) ->
  erlang:error({yecc_bug, "1.4", {missing_state_in_action_table, Other}}).

yeccpars2_0(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, apply, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, export, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, match, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, public, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, variable, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 27, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_0(S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_0/7}).

-compile({nowarn_unused_function, yeccpars2_0/7}).

yeccpars2_cont_0(S, atom, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, float, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, integer, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, string, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(S, '~a', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);
yeccpars2_cont_0(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_1/7}).

-compile({nowarn_unused_function, yeccpars2_1/7}).

yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_1_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_2/7}).

-compile({nowarn_unused_function, yeccpars2_2/7}).

yeccpars2_2(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 112, Ss, Stack, T, Ts, Tzr);
yeccpars2_2(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_3/7}).

-compile({nowarn_unused_function, yeccpars2_3/7}).

yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_3_(Stack),
  yeccgoto_type_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_4/7}).

-compile({nowarn_unused_function, yeccpars2_4/7}).

yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_4_(Stack),
  yeccgoto_program(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_5/7}).

-compile({nowarn_unused_function, yeccpars2_5/7}).

yeccpars2_5(S, variable, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 110, Ss, Stack, T, Ts, Tzr);

yeccpars2_5(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_5_(Stack),
  yeccgoto_type_applications(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_6/7}).

-compile({nowarn_unused_function, yeccpars2_6/7}).

yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_6_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_7/7}).

-compile({nowarn_unused_function, yeccpars2_7/7}).

yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_7_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_8/7}).

-compile({nowarn_unused_function, yeccpars2_8/7}).

yeccpars2_8(_S, '$end', _Ss, Stack, _T, _Ts, _Tzr) -> {ok, hd(Stack)};
yeccpars2_8(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_9/7}).

-compile({nowarn_unused_function, yeccpars2_9/7}).

yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_9_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_10/7}).

-compile({nowarn_unused_function, yeccpars2_10/7}).

yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_10_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_11/7}).

-compile({nowarn_unused_function, yeccpars2_11/7}).

yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_11_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_12/7}).

-compile({nowarn_unused_function, yeccpars2_12/7}).

yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_12_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_13/7}).

-compile({nowarn_unused_function, yeccpars2_13/7}).

yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_13_(Stack),
  yeccgoto_application(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_14/7}).

-compile({nowarn_unused_function, yeccpars2_14/7}).

yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_14_(Stack),
  yeccgoto_program(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_15/7}).

-compile({nowarn_unused_function, yeccpars2_15/7}).

yeccpars2_15(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, apply, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, atom, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, export, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, float, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, integer, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, match, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, public, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, string, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, variable, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_15(S, '~a', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);

yeccpars2_15(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_15_(Stack),
  yeccgoto_applications(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).


yeccpars2_16(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, ']', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 106, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_16(S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_17/7}).

-compile({nowarn_unused_function, yeccpars2_17/7}).

yeccpars2_17(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 89, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, operator, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 90, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(S, '~', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 91, Ss, Stack, T, Ts, Tzr);
yeccpars2_17(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_18/7}).

-compile({nowarn_unused_function, yeccpars2_18/7}).

yeccpars2_18(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_18_(Stack),
  yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_19/7}).

-compile({nowarn_unused_function, yeccpars2_19/7}).

yeccpars2_19(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 86, Ss, Stack, T, Ts, Tzr);
yeccpars2_19(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_20/7}).

-compile({nowarn_unused_function, yeccpars2_20/7}).

yeccpars2_20(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 85, Ss, Stack, T, Ts, Tzr);
yeccpars2_20(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_21/7}).

-compile({nowarn_unused_function, yeccpars2_21/7}).

yeccpars2_21(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_21_(Stack),
  yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_22/7}).

-compile({nowarn_unused_function, yeccpars2_22/7}).

yeccpars2_22(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 66, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(S, left_parenthesis, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 67, Ss, Stack, T, Ts, Tzr);
yeccpars2_22(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_23/7}).

-compile({nowarn_unused_function, yeccpars2_23/7}).

yeccpars2_23(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_23_(Stack),
  yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_24/7}).

-compile({nowarn_unused_function, yeccpars2_24/7}).

yeccpars2_24(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 49, Ss, Stack, T, Ts, Tzr);
yeccpars2_24(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_25/7}).

-compile({nowarn_unused_function, yeccpars2_25/7}).

yeccpars2_25(S, function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_25(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_26/7}).

-compile({nowarn_unused_function, yeccpars2_26/7}).

yeccpars2_26(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_26_(Stack),
  yeccgoto_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_27/7}).

-compile({nowarn_unused_function, yeccpars2_27/7}).

yeccpars2_27(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 39, Ss, Stack, T, Ts, Tzr);
yeccpars2_27(_, _, _, _, T, _, _) -> yeccerror(T).

yeccpars2_28(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_28(S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_29/7}).

-compile({nowarn_unused_function, yeccpars2_29/7}).

yeccpars2_29(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 30, Ss, Stack, T, Ts, Tzr);
yeccpars2_29(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_30/7}).

-compile({nowarn_unused_function, yeccpars2_30/7}).

yeccpars2_30(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_30_(Stack),
  yeccgoto_value(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_31/7}).

-compile({nowarn_unused_function, yeccpars2_31/7}).

yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_31_(Stack),
  yeccgoto_operation_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_32/7}).

-compile({nowarn_unused_function, yeccpars2_32/7}).

yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_32_(Stack),
  yeccgoto_operation_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_33/7}).

-compile({nowarn_unused_function, yeccpars2_33/7}).

yeccpars2_33(S, '}', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 38, Ss, Stack, T, Ts, Tzr);
yeccpars2_33(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_34/7}).

-compile({nowarn_unused_function, yeccpars2_34/7}).

yeccpars2_34(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, atom, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, float, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, integer, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, string, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_34(S, '~a', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);

yeccpars2_34(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_34_(Stack),
  yeccgoto_operation_values(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_35/7}).

-compile({nowarn_unused_function, yeccpars2_35/7}).

yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_35_(Stack),
  yeccgoto_operation_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_36/7}).

-compile({nowarn_unused_function, yeccpars2_36/7}).

yeccpars2_36(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_36_(Stack),
  yeccgoto_operation_value(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_37/7}).

-compile({nowarn_unused_function, yeccpars2_37/7}).

yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_37_(Stack),
  yeccgoto_operation_values(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_38/7}).

-compile({nowarn_unused_function, yeccpars2_38/7}).

yeccpars2_38(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _ | Nss] = Ss,
  NewStack = yeccpars2_38_(Stack),
  yeccgoto_tuple_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_39/7}).

-compile({nowarn_unused_function, yeccpars2_39/7}).

yeccpars2_39(S, assigment, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(S, type_assigment, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_39(_, _, _, _, T, _, _) -> yeccerror(T).

yeccpars2_40(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, apply, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_40(S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_41/7}).

-compile({nowarn_unused_function, yeccpars2_41/7}).

yeccpars2_41(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 42, Ss, Stack, T, Ts, Tzr);
yeccpars2_41(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_42/7}).

-compile({nowarn_unused_function, yeccpars2_42/7}).

yeccpars2_42(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_42_(Stack),
  yeccgoto_type_variable_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_43/7}).

-compile({nowarn_unused_function, yeccpars2_43/7}).

yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_43_(Stack),
  yeccgoto_variable_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_44/7}).

-compile({nowarn_unused_function, yeccpars2_44/7}).

yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_44_(Stack),
  yeccgoto_variable_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_45/7}).

-compile({nowarn_unused_function, yeccpars2_45/7}).

yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_45_(Stack),
  yeccgoto_variable_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_46/7}).

-compile({nowarn_unused_function, yeccpars2_46/7}).

yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_46_(Stack),
  yeccgoto_variable_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_47/7}).

-compile({nowarn_unused_function, yeccpars2_47/7}).

yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_47_(Stack),
  yeccgoto_variable_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_48/7}).

-compile({nowarn_unused_function, yeccpars2_48/7}).

yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_48_(Stack),
  yeccgoto_public_function_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_49/7}).

-compile({nowarn_unused_function, yeccpars2_49/7}).

yeccpars2_49(S, with, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 50, Ss, Stack, T, Ts, Tzr);
yeccpars2_49(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_50/7}).

-compile({nowarn_unused_function, yeccpars2_50/7}).

yeccpars2_50(S, left_parenthesis, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);
yeccpars2_50(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_51/7}).

-compile({nowarn_unused_function, yeccpars2_51/7}).

yeccpars2_51(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 65, Ss, Stack, T, Ts, Tzr);
yeccpars2_51(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_52/7}).

-compile({nowarn_unused_function, yeccpars2_52/7}).

yeccpars2_52(S, left_parenthesis, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 53, Ss, Stack, T, Ts, Tzr);

yeccpars2_52(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_52_(Stack),
  yeccgoto_clause_declarations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).


yeccpars2_53(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 55, Ss, Stack, T, Ts, Tzr);
yeccpars2_53(S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_54/7}).

-compile({nowarn_unused_function, yeccpars2_54/7}).

yeccpars2_54(S, right_parenthesis, Ss, Stack, T, Ts, Tzr) ->
  yeccpars1(S, 61, Ss, Stack, T, Ts, Tzr);

yeccpars2_54(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_55/7}).

-compile({nowarn_unused_function, yeccpars2_55/7}).

yeccpars2_55(S, right_parenthesis, Ss, Stack, T, Ts, Tzr) ->
  yeccpars1(S, 56, Ss, Stack, T, Ts, Tzr);

yeccpars2_55(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_56/7}).

-compile({nowarn_unused_function, yeccpars2_56/7}).

yeccpars2_56(S, open_function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 57, Ss, Stack, T, Ts, Tzr);
yeccpars2_56(_, _, _, _, T, _, _) -> yeccerror(T).

yeccpars2_57(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, apply, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 17, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 19, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, export, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 20, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 22, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, match, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 24, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, public, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 25, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, variable, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 59, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_57(S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_58/7}).

-compile({nowarn_unused_function, yeccpars2_58/7}).

yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _, _ | Nss] = Ss,
  NewStack = yeccpars2_58_(Stack),
  yeccgoto_clause_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_59/7}).

-compile({nowarn_unused_function, yeccpars2_59/7}).

yeccpars2_59(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 60, Ss, Stack, T, Ts, Tzr);
yeccpars2_59(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_60/7}).

-compile({nowarn_unused_function, yeccpars2_60/7}).

yeccpars2_60(S, assigment, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 40, Ss, Stack, T, Ts, Tzr);
yeccpars2_60(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_61/7}).

-compile({nowarn_unused_function, yeccpars2_61/7}).

yeccpars2_61(S, open_function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 62, Ss, Stack, T, Ts, Tzr);
yeccpars2_61(_, _, _, _, T, _, _) -> yeccerror(T).

%% yeccpars2_62: see yeccpars2_57

-dialyzer({nowarn_function, yeccpars2_63/7}).

-compile({nowarn_unused_function, yeccpars2_63/7}).

yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _, _ | Nss] = Ss,
  NewStack = yeccpars2_63_(Stack),
  yeccgoto_clause_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_64/7}).

-compile({nowarn_unused_function, yeccpars2_64/7}).

yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_64_(Stack),
  yeccgoto_clause_declarations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_65/7}).

-compile({nowarn_unused_function, yeccpars2_65/7}).

yeccpars2_65(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _, _ | Nss] = Ss,
  NewStack = yeccpars2_65_(Stack),
  yeccgoto_match_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_66/7}).

-compile({nowarn_unused_function, yeccpars2_66/7}).

yeccpars2_66(S, left_parenthesis, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 75, Ss, Stack, T, Ts, Tzr);
yeccpars2_66(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_67/7}).

-compile({nowarn_unused_function, yeccpars2_67/7}).

yeccpars2_67(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);
yeccpars2_67(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_68/7}).

-compile({nowarn_unused_function, yeccpars2_68/7}).

yeccpars2_68(S, right_parenthesis, Ss, Stack, T, Ts, Tzr) ->
  yeccpars1(S, 71, Ss, Stack, T, Ts, Tzr);

yeccpars2_68(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_69/7}).

-compile({nowarn_unused_function, yeccpars2_69/7}).

yeccpars2_69(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);

yeccpars2_69(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  NewStack = yeccpars2_69_(Stack),
  yeccgoto_declarations(hd(Ss), Cat, Ss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_70/7}).

-compile({nowarn_unused_function, yeccpars2_70/7}).

yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_70_(Stack),
  yeccgoto_declarations(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_71/7}).

-compile({nowarn_unused_function, yeccpars2_71/7}).

yeccpars2_71(S, open_function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 72, Ss, Stack, T, Ts, Tzr);
yeccpars2_71(_, _, _, _, T, _, _) -> yeccerror(T).

%% yeccpars2_72: see yeccpars2_57

-dialyzer({nowarn_function, yeccpars2_73/7}).

-compile({nowarn_unused_function, yeccpars2_73/7}).

yeccpars2_73(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 74, Ss, Stack, T, Ts, Tzr);
yeccpars2_73(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_74/7}).

-compile({nowarn_unused_function, yeccpars2_74/7}).

yeccpars2_74(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _, _, _, _ | Nss] = Ss,
  NewStack = yeccpars2_74_(Stack),
  yeccgoto_function_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_75/7}).

-compile({nowarn_unused_function, yeccpars2_75/7}).

yeccpars2_75(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 69, Ss, Stack, T, Ts, Tzr);

yeccpars2_75(S, right_parenthesis, Ss, Stack, T, Ts, Tzr) ->
  yeccpars1(S, 77, Ss, Stack, T, Ts, Tzr);

yeccpars2_75(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_76/7}).

-compile({nowarn_unused_function, yeccpars2_76/7}).

yeccpars2_76(S, right_parenthesis, Ss, Stack, T, Ts, Tzr) ->
  yeccpars1(S, 81, Ss, Stack, T, Ts, Tzr);

yeccpars2_76(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_77/7}).

-compile({nowarn_unused_function, yeccpars2_77/7}).

yeccpars2_77(S, open_function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 78, Ss, Stack, T, Ts, Tzr);
yeccpars2_77(_, _, _, _, T, _, _) -> yeccerror(T).

%% yeccpars2_78: see yeccpars2_57

-dialyzer({nowarn_function, yeccpars2_79/7}).

-compile({nowarn_unused_function, yeccpars2_79/7}).

yeccpars2_79(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 80, Ss, Stack, T, Ts, Tzr);
yeccpars2_79(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_80/7}).

-compile({nowarn_unused_function, yeccpars2_80/7}).

yeccpars2_80(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _, _, _, _ | Nss] = Ss,
  NewStack = yeccpars2_80_(Stack),
  yeccgoto_function_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_81/7}).

-compile({nowarn_unused_function, yeccpars2_81/7}).

yeccpars2_81(S, open_function, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 82, Ss, Stack, T, Ts, Tzr);
yeccpars2_81(_, _, _, _, T, _, _) -> yeccerror(T).

%% yeccpars2_82: see yeccpars2_57

-dialyzer({nowarn_function, yeccpars2_83/7}).

-compile({nowarn_unused_function, yeccpars2_83/7}).

yeccpars2_83(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 84, Ss, Stack, T, Ts, Tzr);
yeccpars2_83(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_84/7}).

-compile({nowarn_unused_function, yeccpars2_84/7}).

yeccpars2_84(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _, _, _, _, _ | Nss] = Ss,
  NewStack = yeccpars2_84_(Stack),
  yeccgoto_function_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_85/7}).

-compile({nowarn_unused_function, yeccpars2_85/7}).

yeccpars2_85(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_85_(Stack),
  yeccgoto_export_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_86/7}).

-compile({nowarn_unused_function, yeccpars2_86/7}).

yeccpars2_86(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_86_(Stack),
  yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_87/7}).

-compile({nowarn_unused_function, yeccpars2_87/7}).

yeccpars2_87(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 104, Ss, Stack, T, Ts, Tzr);
yeccpars2_87(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_88/7}).

-compile({nowarn_unused_function, yeccpars2_88/7}).

yeccpars2_88(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 103, Ss, Stack, T, Ts, Tzr);
yeccpars2_88(_, _, _, _, T, _, _) -> yeccerror(T).

yeccpars2_89(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 101, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, dot, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_89(S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_cont_0(S, Cat, Ss, Stack, T, Ts, Tzr).

%% yeccpars2_90: see yeccpars2_28

-dialyzer({nowarn_function, yeccpars2_91/7}).

-compile({nowarn_unused_function, yeccpars2_91/7}).

yeccpars2_91(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 93, Ss, Stack, T, Ts, Tzr);
yeccpars2_91(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_92/7}).

-compile({nowarn_unused_function, yeccpars2_92/7}).

yeccpars2_92(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 97, Ss, Stack, T, Ts, Tzr);
yeccpars2_92(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_93/7}).

-compile({nowarn_unused_function, yeccpars2_93/7}).

yeccpars2_93(S, dot, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 94, Ss, Stack, T, Ts, Tzr);
yeccpars2_93(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_94/7}).

-compile({nowarn_unused_function, yeccpars2_94/7}).

yeccpars2_94(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 95, Ss, Stack, T, Ts, Tzr);
yeccpars2_94(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_95/7}).

-compile({nowarn_unused_function, yeccpars2_95/7}).

yeccpars2_95(S, '[', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 16, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, atom, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 18, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 36, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, float, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 21, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, integer, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 23, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, string, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 26, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, '{', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 28, Ss, Stack, T, Ts, Tzr);
yeccpars2_95(S, '~a', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 29, Ss, Stack, T, Ts, Tzr);

yeccpars2_95(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _ | Nss] = Ss,
  NewStack = yeccpars2_95_(Stack),
  yeccgoto_module_function_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_96/7}).

-compile({nowarn_unused_function, yeccpars2_96/7}).

yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_96_(Stack),
  yeccgoto_module_function_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_97/7}).

-compile({nowarn_unused_function, yeccpars2_97/7}).

yeccpars2_97(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_97_(Stack),
  yeccgoto_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

%% yeccpars2_98: see yeccpars2_28

-dialyzer({nowarn_function, yeccpars2_99/7}).

-compile({nowarn_unused_function, yeccpars2_99/7}).

yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _ | Nss] = Ss,
  NewStack = yeccpars2_99_(Stack),
  yeccgoto_operation(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_100/7}).

-compile({nowarn_unused_function, yeccpars2_100/7}).

yeccpars2_100(S, done, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 102, Ss, Stack, T, Ts, Tzr);
yeccpars2_100(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_101/7}).

-compile({nowarn_unused_function, yeccpars2_101/7}).

yeccpars2_101(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _ | Nss] = Ss,
  NewStack = yeccpars2_101_(Stack),
  yeccgoto_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_102/7}).

-compile({nowarn_unused_function, yeccpars2_102/7}).

yeccpars2_102(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _, _ | Nss] = Ss,
  NewStack = yeccpars2_102_(Stack),
  yeccgoto_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_103/7}).

-compile({nowarn_unused_function, yeccpars2_103/7}).

yeccpars2_103(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _ | Nss] = Ss,
  NewStack = yeccpars2_103_(Stack),
  yeccgoto_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_104/7}).

-compile({nowarn_unused_function, yeccpars2_104/7}).

yeccpars2_104(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _ | Nss] = Ss,
  NewStack = yeccpars2_104_(Stack),
  yeccgoto_call(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_105/7}).

-compile({nowarn_unused_function, yeccpars2_105/7}).

yeccpars2_105(S, ']', Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 107, Ss, Stack, T, Ts, Tzr);
yeccpars2_105(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_106/7}).

-compile({nowarn_unused_function, yeccpars2_106/7}).

yeccpars2_106(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_106_(Stack),
  yeccgoto_list_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_107/7}).

-compile({nowarn_unused_function, yeccpars2_107/7}).

yeccpars2_107(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_, _ | Nss] = Ss,
  NewStack = yeccpars2_107_(Stack),
  yeccgoto_list_declaration(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_108/7}).

-compile({nowarn_unused_function, yeccpars2_108/7}).

yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_108_(Stack),
  yeccgoto_applications(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_109/7}).

-compile({nowarn_unused_function, yeccpars2_109/7}).

yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_109_(Stack),
  yeccgoto_type_applications(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccpars2_110/7}).

-compile({nowarn_unused_function, yeccpars2_110/7}).

yeccpars2_110(S, declaration, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 111, Ss, Stack, T, Ts, Tzr);
yeccpars2_110(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_111/7}).

-compile({nowarn_unused_function, yeccpars2_111/7}).

yeccpars2_111(S, type_assigment, Ss, Stack, T, Ts, Tzr) -> yeccpars1(S, 41, Ss, Stack, T, Ts, Tzr);
yeccpars2_111(_, _, _, _, T, _, _) -> yeccerror(T).

-dialyzer({nowarn_function, yeccpars2_112/7}).

-compile({nowarn_unused_function, yeccpars2_112/7}).

yeccpars2_112(_S, Cat, Ss, Stack, T, Ts, Tzr) ->
  [_ | Nss] = Ss,
  NewStack = yeccpars2_112_(Stack),
  yeccgoto_application(hd(Nss), Cat, Nss, NewStack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_application/7}).

-compile({nowarn_unused_function, yeccgoto_application/7}).

yeccgoto_application(0, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_application(15, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_application(57, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_application(62, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_application(72, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_application(78, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_application(82, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_15(15, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_applications/7}).

-compile({nowarn_unused_function, yeccgoto_applications/7}).

yeccgoto_applications(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_14(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_applications(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_108(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_applications(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_58(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_applications(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_63(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_applications(72, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_73(73, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_applications(78, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_79(79, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_applications(82, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_83(83, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_call/7}).

-compile({nowarn_unused_function, yeccgoto_call/7}).

yeccgoto_call(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call(40 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_47(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_call(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_13(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_clause_declaration/7}).

yeccgoto_clause_declaration(50, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_clause_declaration(52, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_52(52, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_clause_declarations/7}).

-compile({nowarn_unused_function, yeccgoto_clause_declarations/7}).

yeccgoto_clause_declarations(50, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_51(51, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_clause_declarations(52 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_64(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_declarations/7}).

-compile({nowarn_unused_function, yeccgoto_declarations/7}).

yeccgoto_declarations(67, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_68(68, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_declarations(69 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_70(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_declarations(75, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_76(76, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_export_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_export_declaration/7}).

yeccgoto_export_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_export_declaration(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_export_declaration(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_export_declaration(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_export_declaration(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_export_declaration(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_export_declaration(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_12(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_function_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_function_declaration/7}).

yeccgoto_function_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(25 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_48(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(40 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_46(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_function_declaration(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_11(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_list_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_list_declaration/7}).

yeccgoto_list_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(16 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(34 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(40 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_45(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_10(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(89 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(90 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_list_declaration(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_35(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_match_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_match_declaration/7}).

yeccgoto_match_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_match_declaration(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_match_declaration(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_match_declaration(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_match_declaration(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_match_declaration(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_match_declaration(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_9(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_module_function_call/7}).

-compile({nowarn_unused_function, yeccgoto_module_function_call/7}).

yeccgoto_module_function_call(17, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_88(88, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_module_function_call(91, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_92(92, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation/7}).

-compile({nowarn_unused_function, yeccgoto_operation/7}).

yeccgoto_operation(17, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_87(87, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_value/7}).

-compile({nowarn_unused_function, yeccgoto_operation_value/7}).

yeccgoto_operation_value(16, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_value(28, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_value(34, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_value(89, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_value(90, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_28(98, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_value(95, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_34(34, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_value(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_99(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_operation_values/7}).

-compile({nowarn_unused_function, yeccgoto_operation_values/7}).

yeccgoto_operation_values(16, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_105(105, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_values(28, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_33(33, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_values(34 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_37(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_values(89, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_100(100, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_operation_values(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_96(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_program/7}).

-compile({nowarn_unused_function, yeccgoto_program/7}).

yeccgoto_program(0, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_8(8, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_public_function_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_public_function_declaration/7}).

yeccgoto_public_function_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_public_function_declaration(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_public_function_declaration(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_public_function_declaration(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_public_function_declaration(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_public_function_declaration(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_public_function_declaration(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_7(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_tuple_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_tuple_declaration/7}).

yeccgoto_tuple_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(16 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(34 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(40 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_44(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_6(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(89 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(90 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_tuple_declaration(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_32(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_application/7}).

-compile({nowarn_unused_function, yeccgoto_type_application/7}).

yeccgoto_type_application(0, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_type_application(5, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_5(5, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_applications/7}).

-compile({nowarn_unused_function, yeccgoto_type_applications/7}).

yeccgoto_type_applications(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_4(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_type_applications(5 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_109(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_type_variable_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_type_variable_declaration/7}).

yeccgoto_type_variable_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_type_variable_declaration(5 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_3(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_value/7}).

-compile({nowarn_unused_function, yeccgoto_value/7}).

yeccgoto_value(0, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(15, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(16 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(28 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(34 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(40 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_43(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(53, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_54(54, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(57, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(62, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(72, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(78, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(82, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_2(2, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(89 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(90 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(95 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr);
yeccgoto_value(98 = _S, Cat, Ss, Stack, T, Ts, Tzr) -> yeccpars2_31(_S, Cat, Ss, Stack, T, Ts, Tzr).

-dialyzer({nowarn_function, yeccgoto_variable_declaration/7}).

-compile({nowarn_unused_function, yeccgoto_variable_declaration/7}).

yeccgoto_variable_declaration(0 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_variable_declaration(15 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_variable_declaration(57 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_variable_declaration(62 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_variable_declaration(72 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_variable_declaration(78 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr);

yeccgoto_variable_declaration(82 = _S, Cat, Ss, Stack, T, Ts, Tzr) ->
  yeccpars2_1(_S, Cat, Ss, Stack, T, Ts, Tzr).

-compile({inline, yeccpars2_1_/1}).

-dialyzer({nowarn_function, yeccpars2_1_/1}).

-compile({nowarn_unused_function, yeccpars2_1_/1}).

-file("chico_parser.yrl", 68).

yeccpars2_1_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_3_/1}).

-dialyzer({nowarn_function, yeccpars2_3_/1}).

-compile({nowarn_unused_function, yeccpars2_3_/1}).

-file("chico_parser.yrl", 59).

yeccpars2_3_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_4_/1}).

-dialyzer({nowarn_function, yeccpars2_4_/1}).

-compile({nowarn_unused_function, yeccpars2_4_/1}).

-file("chico_parser.yrl", 54).

yeccpars2_4_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_5_/1}).

-dialyzer({nowarn_function, yeccpars2_5_/1}).

-compile({nowarn_unused_function, yeccpars2_5_/1}).

-file("chico_parser.yrl", 56).

yeccpars2_5_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin [___1] end | __Stack].

-compile({inline, yeccpars2_6_/1}).

-dialyzer({nowarn_function, yeccpars2_6_/1}).

-compile({nowarn_unused_function, yeccpars2_6_/1}).

-file("chico_parser.yrl", 64).

yeccpars2_6_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_7_/1}).

-dialyzer({nowarn_function, yeccpars2_7_/1}).

-compile({nowarn_unused_function, yeccpars2_7_/1}).

-file("chico_parser.yrl", 67).

yeccpars2_7_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_9_/1}).

-dialyzer({nowarn_function, yeccpars2_9_/1}).

-compile({nowarn_unused_function, yeccpars2_9_/1}).

-file("chico_parser.yrl", 72).

yeccpars2_9_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_10_/1}).

-dialyzer({nowarn_function, yeccpars2_10_/1}).

-compile({nowarn_unused_function, yeccpars2_10_/1}).

-file("chico_parser.yrl", 65).

yeccpars2_10_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_11_/1}).

-dialyzer({nowarn_function, yeccpars2_11_/1}).

-compile({nowarn_unused_function, yeccpars2_11_/1}).

-file("chico_parser.yrl", 66).

yeccpars2_11_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_12_/1}).

-dialyzer({nowarn_function, yeccpars2_12_/1}).

-compile({nowarn_unused_function, yeccpars2_12_/1}).

-file("chico_parser.yrl", 73).

yeccpars2_12_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_13_/1}).

-dialyzer({nowarn_function, yeccpars2_13_/1}).

-compile({nowarn_unused_function, yeccpars2_13_/1}).

-file("chico_parser.yrl", 71).

yeccpars2_13_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_14_/1}).

-dialyzer({nowarn_function, yeccpars2_14_/1}).

-compile({nowarn_unused_function, yeccpars2_14_/1}).

-file("chico_parser.yrl", 53).

yeccpars2_14_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_15_/1}).

-dialyzer({nowarn_function, yeccpars2_15_/1}).

-compile({nowarn_unused_function, yeccpars2_15_/1}).

-file("chico_parser.yrl", 61).

yeccpars2_15_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin [___1] end | __Stack].

-compile({inline, yeccpars2_18_/1}).

-dialyzer({nowarn_function, yeccpars2_18_/1}).

-compile({nowarn_unused_function, yeccpars2_18_/1}).

-file("chico_parser.yrl", 97).

yeccpars2_18_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_21_/1}).

-dialyzer({nowarn_function, yeccpars2_21_/1}).

-compile({nowarn_unused_function, yeccpars2_21_/1}).

-file("chico_parser.yrl", 94).

yeccpars2_21_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_23_/1}).

-dialyzer({nowarn_function, yeccpars2_23_/1}).

-compile({nowarn_unused_function, yeccpars2_23_/1}).

-file("chico_parser.yrl", 95).

yeccpars2_23_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_26_/1}).

-dialyzer({nowarn_function, yeccpars2_26_/1}).

-compile({nowarn_unused_function, yeccpars2_26_/1}).

-file("chico_parser.yrl", 96).

yeccpars2_26_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_30_/1}).

-dialyzer({nowarn_function, yeccpars2_30_/1}).

-compile({nowarn_unused_function, yeccpars2_30_/1}).

-file("chico_parser.yrl", 98).

yeccpars2_30_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin {atom, ___2} end | __Stack].

-compile({inline, yeccpars2_31_/1}).

-dialyzer({nowarn_function, yeccpars2_31_/1}).

-compile({nowarn_unused_function, yeccpars2_31_/1}).

-file("chico_parser.yrl", 103).

yeccpars2_31_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_32_/1}).

-dialyzer({nowarn_function, yeccpars2_32_/1}).

-compile({nowarn_unused_function, yeccpars2_32_/1}).

-file("chico_parser.yrl", 101).

yeccpars2_32_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_34_/1}).

-dialyzer({nowarn_function, yeccpars2_34_/1}).

-compile({nowarn_unused_function, yeccpars2_34_/1}).

-file("chico_parser.yrl", 105).

yeccpars2_34_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin [___1] end | __Stack].

-compile({inline, yeccpars2_35_/1}).

-dialyzer({nowarn_function, yeccpars2_35_/1}).

-compile({nowarn_unused_function, yeccpars2_35_/1}).

-file("chico_parser.yrl", 100).

yeccpars2_35_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_36_/1}).

-dialyzer({nowarn_function, yeccpars2_36_/1}).

-compile({nowarn_unused_function, yeccpars2_36_/1}).

-file("chico_parser.yrl", 102).

yeccpars2_36_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_37_/1}).

-dialyzer({nowarn_function, yeccpars2_37_/1}).

-compile({nowarn_unused_function, yeccpars2_37_/1}).

-file("chico_parser.yrl", 106).

yeccpars2_37_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin [___1 | ___2] end | __Stack].

-compile({inline, yeccpars2_38_/1}).

-dialyzer({nowarn_function, yeccpars2_38_/1}).

-compile({nowarn_unused_function, yeccpars2_38_/1}).

-file("chico_parser.yrl", 151).

yeccpars2_38_(__Stack0) ->
  [___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {tuple, ___2} end | __Stack].

-compile({inline, yeccpars2_42_/1}).

-dialyzer({nowarn_function, yeccpars2_42_/1}).

-compile({nowarn_unused_function, yeccpars2_42_/1}).

-file("chico_parser.yrl", 116).

yeccpars2_42_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {type_var_declaration, ___1, ___2, ___4} end | __Stack].

-compile({inline, yeccpars2_43_/1}).

-dialyzer({nowarn_function, yeccpars2_43_/1}).

-compile({nowarn_unused_function, yeccpars2_43_/1}).

-file("chico_parser.yrl", 110).

yeccpars2_43_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___4} end | __Stack].

-compile({inline, yeccpars2_44_/1}).

-dialyzer({nowarn_function, yeccpars2_44_/1}).

-compile({nowarn_unused_function, yeccpars2_44_/1}).

-file("chico_parser.yrl", 113).

yeccpars2_44_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___4} end | __Stack].

-compile({inline, yeccpars2_45_/1}).

-dialyzer({nowarn_function, yeccpars2_45_/1}).

-compile({nowarn_unused_function, yeccpars2_45_/1}).

-file("chico_parser.yrl", 114).

yeccpars2_45_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___4} end | __Stack].

-compile({inline, yeccpars2_46_/1}).

-dialyzer({nowarn_function, yeccpars2_46_/1}).

-compile({nowarn_unused_function, yeccpars2_46_/1}).

-file("chico_parser.yrl", 112).

yeccpars2_46_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___4} end | __Stack].

-compile({inline, yeccpars2_47_/1}).

-dialyzer({nowarn_function, yeccpars2_47_/1}).

-compile({nowarn_unused_function, yeccpars2_47_/1}).

-file("chico_parser.yrl", 111).

yeccpars2_47_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___4} end | __Stack].

-compile({inline, yeccpars2_48_/1}).

-dialyzer({nowarn_function, yeccpars2_48_/1}).

-compile({nowarn_unused_function, yeccpars2_48_/1}).

-file("chico_parser.yrl", 121).

yeccpars2_48_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin {public, ___2} end | __Stack].

-compile({inline, yeccpars2_52_/1}).

-dialyzer({nowarn_function, yeccpars2_52_/1}).

-compile({nowarn_unused_function, yeccpars2_52_/1}).

-file("chico_parser.yrl", 91).

yeccpars2_52_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin [___1] end | __Stack].

-compile({inline, yeccpars2_58_/1}).

-dialyzer({nowarn_function, yeccpars2_58_/1}).

-compile({nowarn_unused_function, yeccpars2_58_/1}).

-file("chico_parser.yrl", 89).

yeccpars2_58_(__Stack0) ->
  [___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {{guard, ___2}, ___5} end | __Stack].

-compile({inline, yeccpars2_63_/1}).

-dialyzer({nowarn_function, yeccpars2_63_/1}).

-compile({nowarn_unused_function, yeccpars2_63_/1}).

-file("chico_parser.yrl", 88).

yeccpars2_63_(__Stack0) ->
  [___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {{guard, ___2}, ___5} end | __Stack].

-compile({inline, yeccpars2_64_/1}).

-dialyzer({nowarn_function, yeccpars2_64_/1}).

-compile({nowarn_unused_function, yeccpars2_64_/1}).

-file("chico_parser.yrl", 92).

yeccpars2_64_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin [___1 | ___2] end | __Stack].

-compile({inline, yeccpars2_65_/1}).

-dialyzer({nowarn_function, yeccpars2_65_/1}).

-compile({nowarn_unused_function, yeccpars2_65_/1}).

-file("chico_parser.yrl", 86).

yeccpars2_65_(__Stack0) ->
  [___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___4} end | __Stack].

-compile({inline, yeccpars2_69_/1}).

-dialyzer({nowarn_function, yeccpars2_69_/1}).

-compile({nowarn_unused_function, yeccpars2_69_/1}).

-file("chico_parser.yrl", 118).

yeccpars2_69_(__Stack0) ->
  [___1 | __Stack] = __Stack0,
  [begin [___1] end | __Stack].

-compile({inline, yeccpars2_70_/1}).

-dialyzer({nowarn_function, yeccpars2_70_/1}).

-compile({nowarn_unused_function, yeccpars2_70_/1}).

-file("chico_parser.yrl", 119).

yeccpars2_70_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin [___1 | ___2] end | __Stack].

-compile({inline, yeccpars2_74_/1}).

-dialyzer({nowarn_function, yeccpars2_74_/1}).

-compile({nowarn_unused_function, yeccpars2_74_/1}).

-file("chico_parser.yrl", 130).

yeccpars2_74_(__Stack0) ->
  [___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___3, ___6} end | __Stack].

-compile({inline, yeccpars2_80_/1}).

-dialyzer({nowarn_function, yeccpars2_80_/1}).

-compile({nowarn_unused_function, yeccpars2_80_/1}).

-file("chico_parser.yrl", 149).

yeccpars2_80_(__Stack0) ->
  [___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, [], ___6} end | __Stack].

-compile({inline, yeccpars2_84_/1}).

-dialyzer({nowarn_function, yeccpars2_84_/1}).

-compile({nowarn_unused_function, yeccpars2_84_/1}).

-file("chico_parser.yrl", 140).

yeccpars2_84_(__Stack0) ->
  [___8, ___7, ___6, ___5, ___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___4, ___7} end | __Stack].

-compile({inline, yeccpars2_85_/1}).

-dialyzer({nowarn_function, yeccpars2_85_/1}).

-compile({nowarn_unused_function, yeccpars2_85_/1}).

-file("chico_parser.yrl", 75).

yeccpars2_85_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin {export, ___2} end | __Stack].

-compile({inline, yeccpars2_86_/1}).

-dialyzer({nowarn_function, yeccpars2_86_/1}).

-compile({nowarn_unused_function, yeccpars2_86_/1}).

-file("chico_parser.yrl", 69).

yeccpars2_86_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-compile({inline, yeccpars2_95_/1}).

-dialyzer({nowarn_function, yeccpars2_95_/1}).

-compile({nowarn_unused_function, yeccpars2_95_/1}).

-file("chico_parser.yrl", 77).

yeccpars2_95_(__Stack0) ->
  [___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {module_function_call, ___1, ___3, []} end | __Stack].

-compile({inline, yeccpars2_96_/1}).

-dialyzer({nowarn_function, yeccpars2_96_/1}).

-compile({nowarn_unused_function, yeccpars2_96_/1}).

-file("chico_parser.yrl", 78).

yeccpars2_96_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {module_function_call, ___1, ___3, ___4} end | __Stack].

-compile({inline, yeccpars2_97_/1}).

-dialyzer({nowarn_function, yeccpars2_97_/1}).

-compile({nowarn_unused_function, yeccpars2_97_/1}).

-file("chico_parser.yrl", 80).

yeccpars2_97_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {apply, ___3} end | __Stack].

-compile({inline, yeccpars2_99_/1}).

-dialyzer({nowarn_function, yeccpars2_99_/1}).

-compile({nowarn_unused_function, yeccpars2_99_/1}).

-file("chico_parser.yrl", 108).

yeccpars2_99_(__Stack0) ->
  [___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {___1, ___2, ___3} end | __Stack].

-compile({inline, yeccpars2_101_/1}).

-dialyzer({nowarn_function, yeccpars2_101_/1}).

-compile({nowarn_unused_function, yeccpars2_101_/1}).

-file("chico_parser.yrl", 84).

yeccpars2_101_(__Stack0) ->
  [___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {apply, ___2, []} end | __Stack].

-compile({inline, yeccpars2_102_/1}).

-dialyzer({nowarn_function, yeccpars2_102_/1}).

-compile({nowarn_unused_function, yeccpars2_102_/1}).

-file("chico_parser.yrl", 83).

yeccpars2_102_(__Stack0) ->
  [___4, ___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {apply, ___2, ___3} end | __Stack].

-compile({inline, yeccpars2_103_/1}).

-dialyzer({nowarn_function, yeccpars2_103_/1}).

-compile({nowarn_unused_function, yeccpars2_103_/1}).

-file("chico_parser.yrl", 81).

yeccpars2_103_(__Stack0) ->
  [___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {apply, ___2} end | __Stack].

-compile({inline, yeccpars2_104_/1}).

-dialyzer({nowarn_function, yeccpars2_104_/1}).

-compile({nowarn_unused_function, yeccpars2_104_/1}).

-file("chico_parser.yrl", 82).

yeccpars2_104_(__Stack0) ->
  [___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {apply, ___2} end | __Stack].

-compile({inline, yeccpars2_106_/1}).

-dialyzer({nowarn_function, yeccpars2_106_/1}).

-compile({nowarn_unused_function, yeccpars2_106_/1}).

-file("chico_parser.yrl", 154).

yeccpars2_106_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin {list, []} end | __Stack].

-compile({inline, yeccpars2_107_/1}).

-dialyzer({nowarn_function, yeccpars2_107_/1}).

-compile({nowarn_unused_function, yeccpars2_107_/1}).

-file("chico_parser.yrl", 153).

yeccpars2_107_(__Stack0) ->
  [___3, ___2, ___1 | __Stack] = __Stack0,
  [begin {list, ___2} end | __Stack].

-compile({inline, yeccpars2_108_/1}).

-dialyzer({nowarn_function, yeccpars2_108_/1}).

-compile({nowarn_unused_function, yeccpars2_108_/1}).

-file("chico_parser.yrl", 62).

yeccpars2_108_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin [___1 | ___2] end | __Stack].

-compile({inline, yeccpars2_109_/1}).

-dialyzer({nowarn_function, yeccpars2_109_/1}).

-compile({nowarn_unused_function, yeccpars2_109_/1}).

-file("chico_parser.yrl", 57).

yeccpars2_109_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin [___1 | ___2] end | __Stack].

-compile({inline, yeccpars2_112_/1}).

-dialyzer({nowarn_function, yeccpars2_112_/1}).

-compile({nowarn_unused_function, yeccpars2_112_/1}).

-file("chico_parser.yrl", 70).

yeccpars2_112_(__Stack0) ->
  [___2, ___1 | __Stack] = __Stack0,
  [begin ___1 end | __Stack].

-file("chico_parser.yrl", 161).
