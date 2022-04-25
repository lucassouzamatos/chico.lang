## Chico.lang

This is a _toy_ functional language. You can't use in production _yet_.

## Overview
Using the shell, you can do basic operations, like this:

```
$ chico> let a = 12
$ 12
$ chico> apply + a 1 done
$ 13
```

Or create and use functions:

```
$ chico>fun T () -> 1 done done
$ #Fun<erl_eval.45.65746770>
$ chico>apply T done
$ 1
```

## Sections
- [Getting started](#getting-started)
- [Documentation](#documentation)
  - [Comments](#comments)
  - [Variable declarations](#variable-declarations)
  - [Applications](#applications)
  - [Function declarations](#function-declarations)

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
# The let specified is for a test
let T = 1
```

### Variable declarations

The variables are declared as described in the example, and allow only integers and floats:

```
let A = 1
let B = 1.425
```

### Applications

The `apply` term is used for call any function or operator:

```
let A = 12

apply + 1 A done
```

### Function declarations

The functions not has return expression declared, then the last expression is the return from the function.

```
fun sum(A B) ->
  apply + A B done
done
```

If you wanna return a closure function, you can return an another function directly:

```
fun builder(Self) ->
  fun Apply (N) ->
    match N with
      (1) -> 1 done
      (_) -> 
        let X = apply - N 1 done
        let Y = apply Self X done

        apply * N Y done
    done
  done
done
```