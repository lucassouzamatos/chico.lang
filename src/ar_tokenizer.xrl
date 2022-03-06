%% leex file structure
 
Definitions.
U = [A-Z]
L = [a-z]
A = ({U}|{L})
F = (\+|-)?[0-9]+\.[0-9]+
I = (\+|-)?[0-9]*
W = [\s\t\n\r] 

Rules.
{W}+ : skip_token.

apply  : {token, {apply, TokenLine, apply}}.
\+     : {token, {operator, TokenLine, '+'}}.
\-     : {token, {operator, TokenLine, '-'}}.
\/     : {token, {operator, TokenLine, '/'}}.
\*     : {token, {operator, TokenLine, '*'}}.

{I}    : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{F}    : {token, {float, TokenLine, list_to_float(TokenChars)}}.

var    : {token, {variable, TokenLine, none}}.
:=     : {token, {assigment, TokenLine, none}}.
{A}+   : {token, {declaration, TokenLine, list_to_atom(TokenChars)}}.

Erlang code.
