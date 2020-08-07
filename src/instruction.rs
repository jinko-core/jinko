//! Instructions are used to represent a single expression/statement in broccoli.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

use crate::return_kind::ReturnKind;

/// Instructions contain a `ReturnKind`, indicating if they are a statement or an
/// expression
pub struct Instruction {
    /// What kind of instruction this should return: Statement or Expression
    return_kind: ReturnKind,
}
