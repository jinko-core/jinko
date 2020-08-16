//! Represents the usage of a variable, for example when returning from
//! a block.

use super::{InstrKind, Instruction};

pub struct Var {
    name: String,
}

impl Var {
    /// Create a new variable usage with the given name
    pub fn new(name: String) -> Var {
        Var { name }
    }
}

impl Instruction for Var {
    fn kind(&self) -> InstrKind {
        InstrKind::Expression
    }
}
