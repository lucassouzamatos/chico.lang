Nonterminals
  program
  application
  operation
.

Terminals
  apply
  operator
  float
  integer
  variable
  assigment
  declaration
.

Rootsymbol
  program
.

program -> application : '$1'.

application -> apply operation : [{apply, '$2'}].

operation -> operator float float : {'$1', '$2', '$3'}.
operation -> operator integer integer : {'$1', '$2', '$3'}.
operation -> operator integer float : {'$1', '$2', '$3'}.
operation -> operator float integer : {'$1', '$2', '$3'}.

Erlang code.

