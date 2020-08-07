# broccoli
`broccoli` is a small and safe interpreted language using the [`stir`](https://github.com/cohenarthur/stir) intermediate representation

# Requirements

* [ ] If a function does not return `void`, its return value should always be used.
* [ ] You can wrap unsound code in `audit` blocks, to make it easier to review.
* [ ] Variables are immutable by default.
* [ ] Tests should be an integral part of the language
** [ ] This includes unit testing
** [ ] This includes mocking
* [ ] The language must be simple

# Syntax

```rust
fn/func do_stuff(x: int) -> int {
    do_other_stuff(x);

    12
}

x = 12;
other_x: int = 12;
mut another_x: int = 35;

if boolean_condition {
    
} elif other_boolean_condition {

} else {

}

x = if bool { value0 } elif other_bool { value1 } else { value2 }

/* Ternaries work too */
x = bool ? value0 : value1;

while boolean_condition {
    do_stuff();
}

loop {
    do_stuff();
}

for elt in array {
    do_stuff(elt);
}

for i in range(0, 12)/0..12 {
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
x = for i in range(0, n) {
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
