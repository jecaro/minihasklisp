# minihasklisp

[![CI][status-png]][status]

Applicative parser [implementation](/src/parser/Parser.hs) along two examples 
of its use:

- [eval-expr](#eval-expr): a REPL that evaluates an arithmetic expression
- [minihasklisp](#minihasklisp-1): a small yet fully functional lisp 
  interpreter

# eval-expr

`eval-expr` is a simple REPL that evaluates arithmetic expressions. It supports 
the following operations on `Double`: addition `+`, substraction `-`, 
multiplication `*`, division `/` and exponentiation (`^`) with the usual 
predecedence and parenthesis.

Example:

```
$ ./eval-expr
1+1
= 2.0
3+8/60+29/60^2+44/60^3
= 3.1415925925925925
```

# minihasklisp

`minihasklisp` is a tiny lisp interpreter.

## Options

`minihasklisp: [-i] [-h] [-e] [file]`

- `-i`: starts a REPL
- `-e`: print the current symbol table after each statement
- `file`: parse and interpret the file then print the result of the final 
  statement
- `-h`: print the help message

`minihasklisp` needs to be called with at least one of the two options:
`-i` or `file`.

## Language

The language is a lisp variant which supports the following types:
- atoms:
    - 64 bits signed integers
    - booleans: `#t` or `#f`
    - strings
- [S-expressions](https://en.wikipedia.org/wiki/S-expression)

## Builtins

### Functions on pairs

#### `cons`

Create a new pair of two values. Usually used to create linked lists.

```scheme
> (cons 1 2)
(1 . 2)
> (cons 1 (cons 2 '()))
(1 2)
> (cons 1 (cons 2 (cons 3 '())))
(1 2 3)
```

As a convention, `'()` is the last element of a linked list. See also, the 
[`quote`](#quote) function.

#### `quote`

Return its argument as data, without trying to evaluate it. A leading `'` is 
syntaxic sugar for `(quote ...)`.

```scheme
> (quote (1 2 3))
(1 2 3)
> 'a-string
a-string
> (+ 1 2)
3
> '(+ 1 2)
(+ 1 2)
```

#### `car`

Return the first element of a pair.

```scheme
> (car (cons 1 2))
1
> (car '(1 2 3))
1
```

#### `cdr`

Return the second element of a pair.

```scheme
> (cdr (cons 1 2))
2
> (cdr '(1 2 3))
(2 3)
```

### Predicates and branching

#### `eq?`

Test for equality. It works only for atoms, comparing a list to anything always 
returns `#f`.

```scheme
> (eq? 1 1)
#t
> (eq? 2 (+ 1 1))
#t
> (eq? 1 2)
#f
> (eq? 'yes 'yes)
#t
> (eq? '(1 2) '(1 2))
#f
```

#### `atom?`

Test if the parameter is an atom.

```scheme
> (atom? #t)
#t
> (atom? 'an-atom)
#t
> (atom? '())
#t
> (atom? (+ 1 2))
#t
> (atom? '(1 2))
#f
```

#### cond

`cond` takes any number of arguments. Each argument is a list of two 
expressions, if the first expression evaluates to true, it evaluates the second 
expression and returns the result. If not it moves to the next argument.

```scheme
> (cond ((eq? 2 (+ 1 1)) 'if-branch) (#t 'else-branch))
if-branch
> (cond (#f 'first-condition) ((eq? 0 1) 'second-condition) (#t 'catch-all-condition))
catch-all-condition
```

### Arithmetic functions

The following arithmetic functions are supported: `+`, `-`, `*`, `div`, `mod` 
and `<`.

```scheme
> (+ 1 (- 2 (* 3 4)))
-9
> (div 9 2)
4
> (mod 9 2)
1
> (< 0 1)
#t
```

### Functions definition

#### lambda

Lambda functions can be defined with the following syntax:

```scheme
> (lambda (x y) (+ x y))
(lambda (x y) (+ x y))
> ((lambda (x y) (+ x y)) 1 2)
3
```

#### define

`define` lets you affect any value to a symbol. It can be be used to define 
functions as well.

```scheme
> (define a 1)
1
> a
1
> (define add (lambda (x y) (+ x y)))
(lambda (x y) (+ x y))
> (add 1 2)
3
> (define (sub x y) (- x y))
(lambda (x y) (- x y))
> (sub 1 2)
-1
```

#### let

`let` takes a list of symbols and values as it first argument and an expression 
as its second argument. It evaluates the expression looking up symbols in its 
first argument if needed.

```scheme
> (let ((one 1) (twice (lambda (x) (* x 2)))) (twice one))
2
```


## Examples

Some code examples can be found in the [test 
suite](/test/minihasklisp/SExprSpec.hs) or in the following files:

- [`examples.scm`](/test/minihasklisp/files/examples.scm): some basic functions 
  and a few higher order functions such as `map`, `filter`, `fold-left` and 
  `fold-right`
- [`fact.scm`](/test/minihasklisp/files/fact.scm): compute the factorial of a 
  number
- [`fib.scm`](/test/minihasklisp/files/fib.scm): get the nth fibonacci number
- [`qsort3.scm`](/test/minihasklisp/files/qsort3.scm): quicksort implementation
- [`sort.scm`](/test/minihasklisp/files/sort.scm): another sorting algorithm

# Hacking

The project can be build with [nix][nix].

Install with:

```bash
$ nix profile install
```

Build with:

```bash
$ nix build
```

Both binaries are then created in `./result/bin`

Hack with:

```bash
$ nix develop
```

You will be dropped in a shell with all the needed tools in scope: `cabal` to 
build the project and `haskell-language-server` for a better developer 
experience.

[nix]: https://nixos.org/
[status-png]: https://github.com/jecaro/wolfram/workflows/CI/badge.svg
[status]: https://github.com/jecaro/wolfram/actions
