%% leex file structure
 
Definitions.
U	= [A-Z]
L	= [a-z]
A	= ({U}|{L})
F = (\+|-)?[0-9]+\.[0-9]+
I = (\+|-)?[0-9]*

Rules.
[\s\t]+ : skip_token.

apply  : {token, {op, TokenLine, apply}}.
\+     : {token, {operator, TokenLine, '+'}}.
\-     : {token, {operator, TokenLine, '-'}}.
\/     : {token, {operator, TokenLine, '/'}}.
\*     : {token, {operator, TokenLine, '*'}}.

{I}    : {token, {number, TokenLine, list_to_integer(TokenChars)}}.
{F}    : {token, {number, TokenLine, list_to_float(TokenChars)}}.

var    : {token, {variable, TokenLine, none}}.
:=     : {token, {assigment, TokenLine, none}}.
{A}+   : {token, {declaration, TokenLine, TokenChars}}.

Erlang code.
