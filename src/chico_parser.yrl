Nonterminals
  program
  application
  applications
  type_application
  type_applications
  operation
  variable_declaration
  operation_value
  operation_values
  value
  function_declaration
  declarations
  call
  match_declaration
  clause_declaration
  clause_declarations
  guard
  export_declaration
  module_function_call
  pair
  type_variable_declaration
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
  match
  with
  export
  dot
  done
  '#'
  type_assigment
.

Rootsymbol program.

program -> applications : '$1'.
program -> type_applications : '$1'.

type_applications -> type_application : ['$1'].
type_applications -> type_application type_applications : ['$1' | '$2'].

type_application -> type_variable_declaration : '$1'.

applications -> application : ['$1'].
applications -> application applications : ['$1' | '$2'].

application -> function_declaration : '$1'.
application -> variable_declaration : '$1'.
application -> declaration done : '$1'.
application -> value done : '$1'.
application -> call : '$1'.
application -> match_declaration : '$1'.
application -> export_declaration : '$1'.
application -> pair : '$1'.

export_declaration -> export declaration : {export, '$2'}.

module_function_call -> declaration dot declaration :  {module_function_call, '$1', '$3', []}.
module_function_call -> declaration dot declaration operation_values :  {module_function_call, '$1', '$3', '$4'}.

call -> apply module_function_call done : {apply, '$2'}.
call -> apply operation done : {apply, '$2'}.
call -> apply declaration operation_values done : {apply, '$2', '$3'}.
call -> apply declaration done : {apply, '$2', []}.

call -> apply : return_error('$1', {internal, "The apply must be has parameters on right side"}).

match_declaration -> match declaration with clause_declarations done : {'$1', '$2', '$4'}.

clause_declaration -> guard open_function applications : {'$1', '$3'}.

clause_declarations -> clause_declaration : ['$1'].
clause_declarations -> clause_declaration clause_declarations : ['$1' | '$2'].

pair -> '#' left_parenthesis value value right_parenthesis : {pair, {'$3', '$4'}}.

guard -> left_parenthesis value right_parenthesis : {guard, '$2'}.
guard -> left_parenthesis declaration right_parenthesis : {guard, '$2'}.

value -> float : '$1'.
value -> integer : '$1'.
value -> string : '$1'.

operation_value -> declaration : '$1'.
operation_value -> value : '$1'.

operation_values -> operation_value : ['$1'].
operation_values -> operation_value operation_values : ['$1' | '$2'].

operation -> operator operation_value operation_value : {'$1', '$2', '$3'}.

variable_declaration -> variable declaration assigment pair : {'$1', '$2', '$4'}.
variable_declaration -> variable declaration assigment value : {'$1', '$2', '$4'}.
variable_declaration -> variable declaration assigment call : {'$1', '$2', '$4'}.
variable_declaration -> variable declaration assigment function_declaration : {'$1', '$2', '$4'}.

type_variable_declaration -> variable declaration type_assigment declaration : {type_var_declaration, '$1', '$2', '$4'}.

declarations -> declaration : ['$1'].
declarations -> declaration declarations : ['$1' | '$2'].

function_declaration -> 
  function 
  left_parenthesis 
  declarations 
  right_parenthesis 
  open_function
  applications
  done : {'$1', '$3', '$6'}.

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
