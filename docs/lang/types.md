# Types

Types are a central concept of the jinko programming language. Most of the
final implementation of the language will rely on the typechecker in order to
provide safe wrappers on data or concepts.

## built-in types

4 types are baked in the interpreter: `bool`, `int`, `float`, `string` and
`char`. Each of these types has associated functions, which can be found in the
standard library.

- `int`

`int` represents all sorts of signed integers. It is 64-bits wide, and can
contain values from -9 223 372 036 854 775 808 to 9 223 372 036 854 775 807.

- `float`

`float` is a double-precision floating point number, similar to C's `double` or
Rust's `f64`.

- `bool`

At the moment, booleans are still builtin types with two associated keywords:
`true` and `false`. Thanks to this chapter, you'll soon see why we plan on
replacing it with the following in our standard library.

```rust
type true;
type false;
type bool(true | false);
```

- `char`

`char`s are simple UTF-8 characters

- `string`

In `jinko`, strings are not simply arrays of characters, at least for now. We
rely on Rust's `String` implementation in order to have the `string` type. This
is subject to change, and will obviously be different once the interpreter is
self-hosted.

## Custom types

"Custom types" refer to types defined by the programmer. You can find a handful
of them in the standard library, such as `jinko`'s optional or result type:

```rust
type Nothing;
type Maybe[T](T | Nothing);

type Ok[T](with: T);
type Err[E](cause: E);
type Result[T, E](Ok[T] | Err[E]);
```

You can find out more about them [here](types/0_custom_types.md).

## Type conditions

When instantiating a value, a type's author can require that you uphold certain
contracts.

[Read more about contract programming in jinko](types/1_contract.md).
