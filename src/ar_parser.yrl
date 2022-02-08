Nonterminals
  program
  application
  operation
.

Terminals
  apply
  operator
  number
  variable
  assigment
  declaration
.

Rootsymbol
  program
.

program -> application : '$1'.

application -> apply operation : [{apply, '$2'}].

operation -> operator number number : {'$1', '$2', '$3'}.

Erlang code.

