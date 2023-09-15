# Design

This document describes some of the choices made when developing jinko, as well as some implementation choices

## Stmts and Exprs

There are two types of instructions in jinko: Those returning the equivalent of `void`,
or `{}`, such as a variable assignation:

```rust
where x = 12; // Returns "void"

func void_func() { // Returns nothing
}

void_func(); // Thus a statement as well
```

And those returning any other type, which must not be ignored. For example, constant
expressions or non-void function calls:

```rust
func return_x(x: int) -> int {
    x // Returns an integer. Notice the lack of semicolon
}

func return_func(x: int) -> func(int) -> int {
    where l = func lambda(x: int) -> int {
        x + 1
    };

    l
} // This returns a lambda taking an int as argument and returning an int
```

Statements are void, while Expressions return something. You cannot ignore a non-void
return value.

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

In jinko, test functions require no attribute (not that it's any better than using
an attribute, it's just simpler for the interpreter).

```rust
test something_but_in_jinko() {
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

## Choosing between func and fn

jinko uses three keywords to define "functions":
* `test` which are unit tests
* `mock` which are function mocks
* `func` which is for functions

`func` was chosen over `fn` because this way, it looks pretty when next to a `test` or
a `mock` :)

## The `Instruction` struct

Instructions are a central part of Jinko. A jinko program is composed of
instructions and functions, which themselves are instructions.
Instructions can be either Statements or Expressions

```rust
where x = 12; // Stmt
x // Expr
```
-> This jinko code simply assigns a variable x and returns it. If you execute it, the
exit-code will be the value assigned to `x`. In that case, 12

An instruction needs to contain "spacial" information (Where is it in the file ? In what
file ?), source (the actual source code, for errors), and a Statement or an Expression
to execute.

## Importing other source files

Since jinko scripts are simple scripts a la Python, where there is no main functions,
importing simply works by "copy/pasting" other source files. There is no distinction
between headers and source files, so it's a bit different from C's preprocessor for
example. However, the syntax is similar

```rust
incl other_script;
/* Functions, Variables in other_script are imported. They're now named
other_script::<function_name>, other_script::<var_name> and so on */

/* This might change in the future - having first class modules which would desugar
to an instance of a custom type with specific fields, so that we would write other_script.var
or other_script.function() */
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

For now, you can also include directories:

```shell
/*
|_ main.jk
|_ std/
    |_ lib.jk       // includes option.jk, result.jk maybe, etc
    |_ option.jk
    |_ result.jk
*/
````

```rust
incl std // Actually includes std/lib.jk
```

I'm not entirely happy with this design yet. It's obviously open to discussion and changes.

## Garbage collection

Memory allocation and collection is done via reference counting. This implies lower stress
on the hardware used to run the program, and is easier to implement, thus keeping
jinko simpler. However, this causes issues when an instance references itself, thus
creating memory leaks.

## FFI

The idea is to mark functions from external shared libraries with the `ext` keyword.

```rust
ext func add(lhs: int, rhs: int) -> int; // This function isn't defined in jinko
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
in my opinion for Jinko, significantly better for a simple reason: Even if Options are
part of the standard library and "included" by default, they do not rely on some obscure
compiler magic: They are just a type. Therefore, they are being understood by the compiler
as a simple type. And I think that keeping `jinko` simple also means keeping the interpreter
simple. Therefore, I think that simply using `Option`s (or some other nomenclature) would
be best.

## The interpreter

The jinko interpreter should keep track of variables and functions. Therefore, at
least two hashmaps are required, one for variables and one for functions. Each of these
elements need to have a unique name to identify them.

## Structure Types

Allowing user defined types makes for cleaner as well as stricter code. One thing that
jinko really has to focus about is "zero-cost types", as in custom types that englobe
a single, other type without penalty. In the mean time, the interpreter should also focus
on simple parsing and fonctionality. Let's split this section in two:

### Simple type parsing

In order to keep the parsing simple, as much code as possible should be reused. At this
point, the parser is already around 1500 lines big, and that's big enough. Types
declarations are similar to function declarations: They have a name, take arguments.
Type instantiations, however are quite different. Let's examine a custom types in C and its
instantiation:

```c
struct custom_type {
    int int_value;
    char some_character;
    float f;
};

// And to initialize it, one way is to do the following
struct custom_type value = { .int_value = 4, .char = 'J', .float = 27.07};
```

This is similar to creating and calling a function, only that the syntax differs. I
believe jinko could keep the same syntax for both custom types and custom functions, as
they aim to achieve the same "kind" of result: A custom, user-made behavior that produces
cleaner and more readable code.

Thus, the following syntax should be adopted at first:

```rust
// This is similar to a function declaration, without a block and with a ``type`` instead of ``fn``
type CustomType(int_value: int, some_character: char, f: float);

// Let's create one
where value = CustomType(int_value: 4, some_character: 'J', f: 27.07);
```

If your types get too big, then just like function definitions, multilines are supported.

```rust
type CustomType(
    int_value: int,
    some_character: char,
    f: float,
);
```

The only difference between a function definition and a type definition is the keyword:
`type` or `func` and no block of code for the type instantiation.

Type instantiations use the `:` token to associate a value with a field, while function
calls do not use any token. For optional/named arguments, function calls use the `=`
token

#### Methods and functions in Jinko

Let's say you define your custom type in C. If you do "object oriented C", you'll probably
end up with the following:

```c
/* Simple linked list node */
struct ll {
    int value;
    struct ll *next;
};

/* Create a new node */
struct ll *ll_new(int value, struct ll *next);

/* Destroy a previously created node */
void ll_del(struct ll *head);

/* Get the next node */
struct ll *ll_next(struct ll *node);

/* Get the value from the node */
int ll_value(struct ll *node);

/* Push some node to the list */
void ll_push(struct ll *node, struct ll *next);

/* Implementation is not important */

int main(void) {
    // Create our head
    struct ll *last = ll_new(3, NULL);
    struct ll *mid = ll_new(2, last);
    struct ll *head = ll_new(1, mid);

    struct ll *rand = ll_new(67, NULL);
    ll_push(head, rand);

    // Make sure the positions and values are correct
    assert(ll_value(ll_next(head)) == 2);

    // We're done with the list
    ll_del(head);
}
```

The "object oriented" approach does not really work in C, and is cumbersome. Compare it
to any actual OOP language, where we could do `head.del()`, or `head.next().value()`.

However, the object oriented approach brings in a lot of complexity, too much for jinko.
A struct model, in a C way should be prefered. But that doesn't mean we can't add
some syntactic sugar to it.

Let's consider a custom type with its "methods" and a simple function.

```rust
// There's already a "default constructor", so we don't need to define new(). Also, there
// is no NULL in jinko
type LinkedList(value: int, next: Option<LinkedList>);

func del(head: LinkedList) {
    /* Walk the whole list, deleting stuff as it comes */
}

func value(node: LinkedList) -> int {
    node.value
}

func next(node: LinkedList) -> LinkedList {
    node.next
}

func push(node: LinkedList, next: LinkedList) {
    /* Some code to add next to the end of the list or whatever */
}

where last = LinkedList(3, None);
where mid = LinkedList(2, Some(last));
where head = LinkedList(1, Some(mid));

head.push(LinkedList(67, None));

/* Or ... */

push(head, LinkedList(67, None));

assert_eq(head.next().value(), 2);

/* Or ... */

assert_eq(value(next(head)), 2);

/* We're done with the list */

head.del();
```

Both calling methods are valid. In the end, to keep things simple, the first argument
will always act similarly to `self` in Rust, or `this` in most OOP languages. This also
means that the following is possible

```rust
func add(a: int, b: int) -> {
    a + b
}

12.add(15);
add(12, 15);
```

So the concept of methods doesn't really exist in Jinko. The calling method is just
syntactic sugar over regular function calling.

### No-cost custom types

Let's say you're using an API, and using some complex custom made function. For example,
`add_friend`, which takes a last name, a first name, and a nickname. Let's say you're not
using an IDE or a language server: You remember the function name, but not the order of
the arguments: Is it `add_friend(first_name, name, nickname)`?
`add_friend(nickname, first_name, name)`?
If the API was designed by a japanese coder, maybe it's `add_friend(name, first_name, nickname)`?

There is no way to know without checking the documentation. Now, this is good, since it
forces you to check the documentation. But we can also enforce type safety and data safety
through the interpreter.

```rust
// The friend type, that already existed
type Friend(/* Some values */);

// Define three "custom" types
type Name(the_name: str);
type FirstName(value: str);
type Nickname(hidden: str);

// And define the API function like so:
func add_friend(name: Name, f_name: FirstName, n_name: Nickname) -> Friend {
    /* Some code */
}
```

## Generics

Generics rely on the use of brackets so that the parser can *easily* differentiate between
a generic use/declaration (`A<T>`) versus a variable comparison (`A < T`) without any sort
of lookahead. Thus, the following in C++

```c++
std::vector<std::unordered_map<std::string, int>> map = ...;
```

would become in jinko

```rust
where map: Vec[Map[string, int]] = ...; 
```

Likewise, the following Rust code

```rust
fn takes_result(r: Result<String, SomeErrorType>) {}
```

becomes

```rust
func takes_result(r: Result[string, SomeErrorType]) {}
```

This syntax is similar to scala's generics, which are quite nice to use.

Generics, just like function parameters, should be able to take a default value, which
could be specified by name.

```rust
type BaseError(inner: string);

type Ok[T](with: T);
type Err[E](from: E);
type Result[T = void, E = BaseError] = Ok[T] | Err[E];

// We can avoid typing `Result` by using type promotion but w/e, this is for
// the example's sake
where ok_0: Result[int] = Ok(with: 15); // E defaults to BaseError
where ok_1: Result[int, string] = Ok(with: 15);
where ok_2: Result[int, string] = Err(from: "oops");
where ok_4: Result[T = int, string] = Err(from: "oops");
where ok_5: Result[T = int, E = string] = Err(from: "oops");
where ok_6: Result[E = bool] = Err(from: false); // T defaults to void
```
