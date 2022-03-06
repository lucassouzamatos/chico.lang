Nonterminals
  program
  application
  applications
  operation
  variable_declaration
  operation_value
  value
  function_declaration
  functions
  declarations
.

Terminals
  apply
  operator
  float
  integer
  variable
  assigment
  declaration
  function
  left_parenthesis
  right_parenthesis
  open_function
.

Rootsymbol
  program
.

program -> applications : '$1'.
program -> functions : '$1'.

functions -> function_declaration : ['$1'].
functions -> function_declaration functions : ['$1' | '$2'].

applications -> application : ['$1'].
applications -> application applications : ['$1' | '$2'].

application -> apply operation : {apply, '$2'}.
application -> variable_declaration : '$1'.

value -> float : '$1'.
value -> integer : '$1'.

operation_value -> declaration : '$1'.
operation_value -> value : '$1'.

operation -> operator operation_value operation_value : {'$1', '$2', '$3'}.

variable_declaration -> variable declaration assigment value : {'$1', '$2', '$4'}.

declarations -> declaration : ['$1'].
declarations -> declaration declarations : ['$1' | '$2'].

function_declaration -> 
  function 
  declaration 
  left_parenthesis 
  declarations 
  right_parenthesis 
  open_function
  applications : {'$1', '$2', '$4', '$7'}.

Erlang code.

