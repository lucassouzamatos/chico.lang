## Arithmetic Interpreter

Interpret basic arithmetic operations.

This lib is in development status.

Usage example:

```bash
$ ar> calc + 2 2 
$ 4
```

## Gramar
The BNF grammar follows this instructions above:

```
<program> ::= <variable> | <calc>

<calc> ::= "calc " <operator> " " <number> " " <number>
<operator> ::= "+" | "-" | "*" | "/"
<number> ::= [0-9]+

<variable> ::= "var " <identifier> " = " <value>
<identifier> ::= ([a-z] | [A-Z])+ 
<value> ::= [0-9]+
```

## Future implementations
For future implementations, follows:

### Variables
On the backlog for this interpreter is the enrivonment language, that can store variables and uses on calculation. For example:
```bash
$ ar> var a = 2, var b = 3, calc + a b.
```

### Code file
Should be possible execute all program by source code file, for example:

```bash
ar math-calc.ar
```
Where the math-calc.ar looks like:
```
# math-calc.ar

var a = 2,
var b = 3,
calc + a b.
```
