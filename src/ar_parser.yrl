Nonterminals
  program
  application
  applications
  operation
  variable_declaration
  operation_value
  operation_values
  value
  function_declaration
  declarations
  call
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
  string
  done
.

Rootsymbol
  program
.

program -> applications : '$1'.

applications -> application : ['$1'].
applications -> application applications : ['$1' | '$2'].

application -> function_declaration : '$1'.
application -> variable_declaration : '$1'.
application -> call : '$1'.

call -> apply operation done : {apply, '$2'}. %% apply + 2 2 done
call -> apply declaration operation_values done : {apply, '$2', '$3'}. %% apply sum 2 2 done
call -> apply declaration done : {apply, '$2', []}. %% apply sum done

value -> float : '$1'.
value -> integer : '$1'.
value -> string : '$1'.

operation_value -> declaration : '$1'.
operation_value -> value : '$1'.

operation_values -> operation_value : ['$1'].
operation_values -> operation_value operation_values : ['$1' | '$2'].

operation -> operator operation_value operation_value : {'$1', '$2', '$3'}.

variable_declaration -> variable declaration assigment value : {'$1', '$2', '$4'}.
variable_declaration -> variable declaration assigment call : {'$1', '$2', '$4'}.

declarations -> declaration : ['$1'].
declarations -> declaration declarations : ['$1' | '$2'].

function_declaration -> 
  function 
  declaration 
  left_parenthesis 
  declarations 
  right_parenthesis 
  open_function
  applications
  done : {'$1', '$2', '$4', '$7'}.

function_declaration -> 
  function 
  declaration 
  left_parenthesis 
  right_parenthesis 
  open_function
  applications
  done : {'$1', '$2', [], '$6'}.
  
Erlang code.

