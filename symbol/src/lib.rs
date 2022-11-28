//! Interned symbol crate. Creating a symbol will either create a new one, or return a
//! "hidden reference" to an existing symbol. This allows reusing allocations and is
//! useful in contexts where the same string might be reused multiple times. This also
//! goes well in combination with a "small string type", one which allows storing the
//! string directly on the stack if it is less than a pointer's width in length. This is
//! quite useful since most names within a program are less than an usual pointer width.

// FIXME: How do we do that? There's two ways to go around the issue:
// 1. Keep a `static mut HashSet`
// 2. Pass a `SymbolCtx` around at all times
//
// The first one is more disgusting, the second one more annoying to use.
// The first one has the advantage of making it easy to initialize with "default" symbols:
// int, float, string...

use std::collections::HashSet;
use std::sync::{Arc, Mutex};

use lazy_static::lazy_static;

lazy_static! {
    static ref SYMBOLS: Mutex<HashSet<Arc<String>>> = {
        macro_rules! builtin {
            ($set:expr, $builtin:literal) => {
                $set.insert(Arc::new(String::from($builtin)))
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Symbol(
    Arc<String>, /* FIXME: Switch to SmolStr or equivalent */
);

impl Symbol {
    /// # Panics
    ///
    /// This function panics if the underlying mutex is poisoned
    #[must_use]
    pub fn new(inner: String) -> Symbol {
        let mut set = SYMBOLS.lock().unwrap();

        // FIXME: When `hash_set_entry` gets stabilized :)
        // Symbol(set.get_or_insert(inner))

        if let Some(reference) = set.get(&inner) {
            Symbol(reference.clone())
        } else {
            let arc = Arc::new(inner);

            set.insert(arc.clone());
            Symbol(arc)
        }
    }

    /// Access a reference to the underlying string contained in a [`Symbol`]
    #[must_use]
    pub fn access(&self) -> &str {
        self.0.as_str()
    }
}

impl From<String> for Symbol {
    fn from(inner: String) -> Symbol {
        Symbol::new(inner)
    }
}

impl From<&str> for Symbol {
    fn from(inner: &str) -> Symbol {
        Symbol::new(String::from(inner))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn insertion() {
        {
            let set = SYMBOLS.lock().unwrap();

            assert!(set.get(&String::from("int")).is_some());
            assert!(set.get(&String::from("zip")).is_none());
        }

        let _sym = Symbol::new(String::from("zip"));

        {
            let set = SYMBOLS.lock().unwrap();

            assert!(set.get(&String::from("zip")).is_some());
        }
    }
}
