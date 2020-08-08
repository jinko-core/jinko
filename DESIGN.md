# Design

This document describes some of the choices made when developing broccoli, as well as some implementation choices

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

## Importing other source files

Since broccoli scripts are simple scripts a la Python, where there is no main functions,
importing simply works by "copy/pasting" other source files. There is no distinction
between headers and source files, so it's a bit different than C's preprocessor for
example. However, the syntax is similar

```rust
incl other_script;
/* Functions, Variables in other_script are imported. They're now named
other_script::<function_name>, other_script::<var_name> and so on */
```

There is no way to remove the usage of `<source_name>::`. This is the cause of some
programming issues in C++, C# and other languages with similar features.

You can only import an external source once per program. This way, no circular
dependencies are created.

To separate sources with the same name, for example from multiple directories, you
can use the `as` syntax

```rust
// Wrong
incl dir0::source0;
incl dir1::source0;

/* Both sources would have the same name. This is an error */

// Good
incl dir0::source0 as dir0_source0;
incl dir1::source0 as dir1_source0;
/* Now, dir0::source0 functions are named dir0_source0::<function_name> and so on */
```

I'm not entirely happy with this design yet. It's obviously open to discussion and changes
