%% leex file structure
 
Definitions.
U = [A-Z]
L = [a-z]
A = ({U}|{L}|_|@)
F = (\+|-)?[0-9]+\.[0-9]+
I = (\+|-)?[0-9]*
W = [\s\t\n\r] 
D = \.

Rules.
{W}+ : skip_token.
{D}    : {token, {dot, TokenLine, none}}.

pub    : {token, {public, TokenLine, none}}.
fun    : {token, {function, TokenLine, none}}.
\(     : {token, {left_parenthesis, TokenLine, none}}.
\)     : {token, {right_parenthesis, TokenLine, none}}.
\->    : {token, {open_function, TokenLine, none}}.
done   : {token, {done, TokenLine, none}}.
match  : {token, {match, TokenLine, none}}.
with   : {token, {with, TokenLine, none}}.

"(\\\^.|\\.|[^\"])*" : {token, {string, TokenLine, atom_to_list(get_string(TokenChars, TokenLen))}}.
'(\\\^.|\\.|[^\"])*' : {token, {atom, TokenLine, get_string(TokenChars, TokenLen)}}.

\#     : {token, {'#', TokenLine, none}}.
\{     : {token, {'{', TokenLine, none}}.
\}     : {token, {'}', TokenLine, none}}.
\[     : {token, {'[', TokenLine, none}}.
\]     : {token, {']', TokenLine, none}}.
\~     : {token, {'~', TokenLine, none}}.
\~a    : {token, {'~a', TokenLine, none}}.

export : {token, {export, TokenLine, export}}.
apply  : {token, {apply, TokenLine, apply}}.
\+     : {token, {operator, TokenLine, '+'}}.
\-     : {token, {operator, TokenLine, '-'}}.
\/     : {token, {operator, TokenLine, '/'}}.
\*     : {token, {operator, TokenLine, '*'}}.


let    : {token, {variable, TokenLine, none}}.
=      : {token, {assigment, TokenLine, none}}.
::     : {token, {type_assigment, TokenLine, none}}.
{A}+   : {token, {declaration, TokenLine, list_to_atom(TokenChars)}}.

{I}    : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{F}    : {token, {float, TokenLine, list_to_float(TokenChars)}}.

_      : {token, {declaration, TokenLine, '_'}}.

Erlang code.

get_string(TokenChars, TokenLen) -> 
  S = lists:sublist(TokenChars, 2, TokenLen - 2), 
  list_to_atom(S).
