//! Interned symbol crate. Creating a symbol will either create a new one, or return a
//! "hidden reference" to an existing symbol. This allows reusing allocations and is
//! useful in contexts where the same string might be reused multiple times. This also
//! goes well in combination with a "small string type", one which allows storing the
//! string directly on the stack if it is less than a pointer's width in length. This is
//! quite useful since most names within a program are less than an usual pointer width

// FIXME: How do we do that? There's two ways to go around the issue:
// 1. Keep a `static mut HashSet`
// 2. Pass a `SymbolCtx` around at all times
//
// The first one is more disgusting, the second one more annoying to use.
// The first one has the advantage of making it easy to initialize with "default" symbols:
// int, float, string...

use std::collections::HashSet;
use std::sync::Mutex;

use lazy_static::lazy_static;

lazy_static! {
    static ref SYMBOL: Mutex<HashSet<String>> = {
        macro_rules! builtin {
            ($set:expr, $builtin:literal) => {
                $set.insert(String::from($builtin))
            };
        }

        let mut set = HashSet::new();

        // All primitive types
        builtin!(set, "int");
        builtin!(set, "float");
        builtin!(set, "bool");
        builtin!(set, "char");
        builtin!(set, "string");
        // Not the `func` keyword, but the builtin function type
        builtin!(set, "func");

        Mutex::new(set)
    };
}

pub struct Symbol(
    &'static String, /* FIXME: Switch to SmolStr or equivalent */
);

impl Symbol {
    #[must_use]
    /// # Panics
    ///
    /// This function panics if the underlying mutex is poisoned
    pub fn new(inner: String) -> Symbol {
        let mut set = SYMBOL.lock().unwrap();

        // FIXME: When `hash_set_entry` gets stabilized :)
        // Symbol(set.get_or_insert(inner))

        todo!()
    }
}
