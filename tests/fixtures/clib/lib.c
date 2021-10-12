// Multiple arguments are passed to native functions via a "vector" of JkInstances. It's
// actually a singular instance that contains multiple instances. FIXME Is it?

// We need to represent any jinko values safely in order to use a singular function
// type when getting functions on the Rust side from the shared library.
// We have to use a Symbol<T>, which isn't flexible at all. It can be a Symbol<fn(i32) -> i32>,
// or Symbol<i32> for a value, but we cannot change the type dynamically (at least not
// simply, and not without reflection).

#include <stdio.h>

int square(int a) {
    return a * a;
}

int no_arg() {
    return 15;
}

int add(int lhs, int rhs) {
    return lhs + rhs;
}

void print_something() {
    printf("jinko called\n");
}
