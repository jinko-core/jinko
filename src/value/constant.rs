//! The constant module symbolizes a constant in the source code. There are 4 different
//! types of constants: Integer, Float, String and Character

use super::Value;

/// The 4 different types of constants
enum ConstKind {
    Char,
    Str,
    Int,
    Float,
}

/// The Constant contains a kind, and the associated value
struct Constant<T: Copy> {
    kind: ConstKind,
    value: T,
}

impl<T: Copy> Value for Constant<T> {
    type Contained = T;

    fn value(&self) -> Self::Contained {
        self.value
    }
}
