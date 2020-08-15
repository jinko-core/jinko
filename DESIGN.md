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

## Garbage collection

Memory allocation and collection is done via reference counting. This implies lower stress
on the hardware used to run the program, and is easier to implement, thus keeping
broccoli simpler. However, this causes issues when an instance references itself, thus
creating memory leaks.

## FFI

The idea is to mark functions from external shared libraries with the `ext` keyword.

```rust
ext func add(lhs: int, rhs: int) -> int; // This function isn't defined in broccoli
```

Calling `add()` will actually make a call into a native-code function, for example one
written in Rust, C or C++. Adjustments need to be done on the native side of things in
order to allow name resolution

## Nullable types

In Rust, types are not nullable. There is no way (in the safe subset of the language
at least) to return `NULL` as a value. However, the `Option` type exists: You can either
return `Some(value)` or `None` in case something went wrong. You also have to handle both
cases when using those Option types. In languages such as Zig (I think) or Dart (soon),
some types can be "nullable". By annotating the type with a question mark, you can
indicate that the value might be null. For example, `String` needs to be a valid string,
but `String?` can be a valid string or NULL. These two approaches do not exactly serve
the same purpose. However, they are useful when it comes to error handling, as well as
the possibility of not having something. In C, you are constrained to use NULL. Every
pointer is "nullable", and therefore you always have to check for NULL. In Dart and Zig,
you only have to check for NULL if the type is nullable. In Rust, you have to check
your option types or `unwrap()` on them, which will cause a panic in case of a `None`
(a bit equivalent to segfaulting on NULL, but less sneaky and way less vulnerable).

While these two approaches both have advantages and inconvenient, the Rust approach is,
in my opinion for Broccoli, significantly better for a simple reason: Even if Options are
part of the standard library and "included" by default, they do not relie on some obscure
compiler magic: They are just a type. Therefore, they are being understood by the compiler
as just a type. And I think that keeping `broccoli` simple also means keeping the interpreter
simple. Therefore, I think that simply using `Option`s (or some other nomenclature) would
be best.

## The interpreter

The broccoli interpreter should keep track of variables and functions. Therefore, at
least two hashmaps are required, one for variables and one for functions. Each of these
elements need to have a unique name to identify them.