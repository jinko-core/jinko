# Syntax

The syntax of `jinko` is strongly inspired by rust's. The aim is to provide a simpler,
slower, interpreted scripting language. Ultimately, prototyping in `jinko` and
`transpiling` to a rust application should be possible.

## Function declaration
```rust
func do_stuff(x: int) -> int {
    do_other_stuff(x);

    12
}
```

## Variable assignment

```rust
x = 12;
x = do_stuff(x);
other_x: int = 12;
mut another_x: int = 35;
```

## Branching

```rust
if boolean_condition {

} else {

}
```

## Ternaries

```rust
x = if bool { value0 } else { value2 }

/* Ternaries work too */
x = bool ? value0 : value1;
```

## Loops

```rust
while boolean_condition {
    do_stuff();
}

loop {
    do_stuff();
}

for elt in array {
    do_stuff(elt);
}

for i in range(0, 12) /* 0..12 */ {
    do_stuff(i);
}

// Assign the result of a for loop to a variable
// The for loop needs to have a return type
// Replaces the following;
//
// mut x: int = 0;
// for elt in array {
//      do_stuff(elt);
//      x = elt;
// }
// x = elt; // Last computation made
x = for elt in array -> int {
    do_stuff(elt);

    elt
}

// There's type inference
x = for i in range(0, n + 1) {
    i
} // x now equals n

// The same goes for `while` and `loop`
x = while boolean_condition -> int {
    if i == 12 {
        i
    }

    i++;
} // x now equals 12
```

## Generics

In order to keep the parser simple, generics use brackets instead of chevrons.

```rust
type Nothing;
type Some[T](T);
type Maybe[T](Nothing | Some[T]);

func takes_maybe[SomeTyName](m: Maybe[SomeTyName]) -> SomeTyName {}

type Ok[T](T);
type Err[T](T);
type Result[T = void, E = Error](Ok[T] | Err[E]);

func returns_result() -> Result[int, SomeErrorType] {}

type Map[K = string, V](...);
```
