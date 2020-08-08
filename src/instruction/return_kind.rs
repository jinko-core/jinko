//! Statements return `Nothing`. Expressions return `Something`. You can ignore `Nothing`s,
//! but not `Something`s.

use crate::value::Value;

/// The two possible return "kinds" when executing an a broccoli instruction
pub enum ReturnKind {
    Nothing,
    Something,
}
