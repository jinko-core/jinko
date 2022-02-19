# Type obligations

When defining a [custom type](custom_types.md), you may wish for your users to
respect certain obligations. Say for example, that you wish to only deal with
even numbers. You would define an `Even` type, containing its associated integer.

```rust
type Even(value: int);
```

This isn't really useful, since you're able to do the following:

```rust
two = Even(value: 3);
```

One solution to this would be to hide the instantiation of this type behind a
function, and telling the user to be very careful and only call this function
to create a new even number:

```rust
func even(from: int) -> Maybe[Even] {
	if from.mod(2) == 0 {
		Even(value: from)
	} else {
		Nothing
	}
}
```

which forces your users to make sure they've received a correct `Even` and not
an instance of `Nothing`.

Since this behavior is extremely useful (ranged integers, specific values out of
multiple possibilities, safety checks...), `jinko` offers a keyword to help
define conditions that users must uphold when instantiating types.

The standard library uses those type obligations in order to make calls to
foreign functions safer: For example, when wrapping the standard C library, you
will often face two situations when dealing with pointers:

- A null pointer is a valid value, and will be checked for (`write(3)`)
- A null pointer is invalid, and will not be checked for: The function might crash (`qsort(3)`)

You can easily represent these two types of pointers in `jinko`: One is simply
a pointer, and the other a non-null pointer. These are not novel concepts:
They've been in various programming languages. However, `jinko`'s type system
helps in making sure that a `Pointer` is never given to a function expecting a
`NonNullPointer`, while still easily allowing you to convert a `NonNullPointer`
into a `Pointer`.

## Defining type obligations

You can define a type obligation by adding the `with` keyword to your type
declaration:

```rust
type Even(value: int) with value.mod(2) == 0;
```

The syntax is as follows:

```rust
'type' <name> '(' <fields> ')' 'with' <boolean_expr> ';'
```

A boolean expression can be any `jinko` expression: It will be typechecked to
make sure that it returns a boolean. This means that you can have multiple checks
on a single type:

```rust
type PositiveEven(value: int) with value.mod(2) == 0 && value > 0;

// or slightly more verbose

type PositiveEven(value: int) with {
	mod_check = value.mod(2) == 0;
	pos_check = value > 0;

	mod_check && pos_check
}
```

## Instantiating types with type obligations

When creating such a type instance, it is important to note to the user that
failure might occur. This is why instantiating those values will not directly return
the type `T`, but `T` wrapped in a `Maybe`:

```rust
a: Even = Even(value: 2); // type error!
// `a` is actually of the type `Maybe[Even]`
```

We must handle the optional type to make sure that we deal with the failure case
appropriately:

```rust
a0: Even = Even(value: 2).unpack(); // if we know it cannot fail
a1: Even = Even(value: 2).try(); // to propagate the error to the caller
```

In the case that the condition fails, an instance of `Nothing` will be returned.

```rust
assert(Even(value: 3).is_nothing()) // true
```

## Making our safe FFI pointer types

As explained above, the problem is as follows: Some pointers are *allowed* to
be null pointers, while some pointers *cannot*.

Let's take the following C example, which sorts an array of integers:

```c
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

int icmp(const void *l_v, const void *r_v) {
  int l = (int)(unsigned long)l_v;
  int r = (int)(unsigned long)r_v;

  return r > l ? 1 : r < l ? -1 : 0;
}

int main(void) {
  int *ptr = NULL;

  int array[5] = {0, 1, 2, 3, 4};

  qsort(array, 5, sizeof(int), icmp);

  for (size_t i = 0; i < 5; i++)
    printf("array[%lu] = %d\n", i, array[i]);

  // No comparison function here
  qsort(array, 5, sizeof(int), NULL);

  return 0;
}
```

In the case that we forget to give a valid comparison function to `qsort`, the
program will crash.

When wrapping the `qsort` function in `jinko`, we might thus want to mark the
last argument of the function as a `NonNullPointer`:

```rust
link_with("libc.so.6");

ext func qsort(base: NonNullPointer, nmemb: int, size: int, cmp: NonNullPointer);
```

This will prevent `jinko` code to *ever* pass a `NULL` pointer to the `qsort`
function.

```rust
type RawPointer(raw_value: int); // ...
type NonNullPointer(ptr: RawPointer) with ptr.raw_value != 0;
```

Since we may want to give `NonNullPointer`s to functions that expect a regular,
nullable `Pointer`, we can use multi-types to defined the `Pointer` type:

```rust
type Pointer(ptr: NonNullPointer | RawPointer);
```

Any value of type `Pointer` will thus be able to be created by upcasting a `NonNullPointer`
or a `RawPointer`.
