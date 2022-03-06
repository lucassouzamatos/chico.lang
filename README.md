## Chico.lang

Interpret basic arithmetic operations.

This lib is in development status.

Usage example:

```bash
$ chico> var a := 12
$ 12
$ chico> apply + a 1
$ 13

```

## Getting started

First step for start is clone the project:

```
$ git clone https://github.com/lucassouzamatos/chico.lang.git
```

After this, you should compile files:

```
$ make
```

And run the application:

```
$ make run
```

## Documentation
Below it's possible see some examples about features enabled in the lang.

### Comments
All lines with `#` in the start are considered a comment.
``` 
# The var specified is for a test
var T := 1
``` 

### Variable declarations
The variables are declared as described in the example, and allow only integers and floats:
```
var A := 1
var B := 1.425
```

### Applications
The `apply` term is used for call any function or operator:
```
var A := 12

apply + 1 A
```

### Function declarations
The functions not has return expression declared, then the last expression is the return from the function.
```
fun sum(A B) -> apply + A B
```
