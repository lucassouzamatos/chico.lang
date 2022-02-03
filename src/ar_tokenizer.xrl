%% leex file structure
 
Definitions.
U	= [A-Z]
L	= [a-z]
A	= ({U}|{L})

Rules.
[\s\t]+ : skip_token.

[0-9]+ : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
apply  : {token, {op, TokenLine, apply}}.
\+     : {token, {operator, TokenLine, '+'}}.
\-     : {token, {operator, TokenLine, '-'}}.
\/     : {token, {operator, TokenLine, '/'}}.
\*     : {token, {operator, TokenLine, '*'}}.
var    : {token, {variable, TokenLine, none}}.
:=     : {token, {assigment, TokenLine, none}}.
{A}+   : {token, {declaration, TokenLine, TokenChars}}.

Erlang code.
