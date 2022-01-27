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
