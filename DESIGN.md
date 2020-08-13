# Design

This document describes some of the choices made when developing broccoli, as well
as some implementation choices

## Stmts and Exprs

There are two types of instructions in broccoli: Those returning the equivalent of `void`,
or `{}`, such as a variable assignation:

```rust
x = 12; // Returns "void"

func void_func() { // Returns nothing
}

void_func(); // Thus a statement as well
```

And those returning any other type, which must not be ignored. For example, constant
expressions or non-void function calls:

```rust
func return_x(int: x) -> int {
    x // Returns an integer. Notice the lack of semicolon
}

func return_func(int: x) -> func(int) -> int {
    l = func lambda(int: x) -> int {
        x + 1
    };

    l
} // This returns a lambda taking an int as argument and returning an int
```

Statements return `Nothing`, while Expressions return `Something`. You cannot ignore
`Something`.

## Unit tests

Embedding unit testing in a language relies on using an "attribute-like" syntax. For
example, in Java you can do the following by using a library, JUnit, which itself uses
attributes (@ syntax).

```java
@Test
public void something_something_factory_bean_sprout() {
    assertEquals(something(), something_else());
}
```

In rust, attributes (or tags) use the #[syntax]. Unit tests are embedded directly into
the language without the need for external libraries.

```rust
#[test]
fn something_in_rust() {
    assert_eq!(something(), something_else());
}
```

In broccoli, test functions require no attribute (not that it's any better than using
an attribute, it's just simpler for the interpreter).

```rust
test something_but_in_broccoli() {
    assert_eq(something(), something_else());
}
```

Mocking is done similarly, by using the `mock` keyword

```rust
/* This will mock the function something() */
mock something() {
    /* Mocking */
}
```

## Choosing between func and func

broccoli uses three keywords to define "functions":
* `test` which are unit tests
* `mock` which are function mocks
* `func` which is for functions and procedures

(Procedures return `Nothing`, while Functions return `Something`)

`func` was chosen over `func` because this way, it looks pretty when next to a `test` or
a `mock` :)

## The `Instruction` struct

Instructions are a central part of Broccoli. A broccoli program is composed of 
instructions and functions, which themselves are instructions.
Instructions can be either Statements or Expressions

```rust
x = 12; // Stmt
x // Expr
```
-> This broccoli code simply assigns a variable x and returns it. If you execute it, the
exit-code will be the value assigned to `x`. In that case, 12

An instruction needs to contain "spacial" information (Where is it in the file ? In what
file ?), source (the actual source code, for errors), and a Statement or an Expression
to execute.

## The interpreter

The broccoli interpreter should keep track of variables and functions. Therefore, at
least two hashmaps are required, one for variables and one for functions. Each of these
elements need to have a unique name to identify them.
