//! The VarAssign struct is used when assigning values to variables.

use crate::value::Constant;

use super::{ Instruction, InstrKind };

pub struct VarAssign {
    /// Is the variable mutable ? This is only useful on variable declaration
    mutable: bool,

    /// The "name" of the variable
    symbol: String,

    value: Constant,
}

impl VarAssign {
    pub fn new(mutable: bool, symbol: String, value: Constant) -> VarAssign {
        VarAssign {
            mutable,
            symbol,
            value,
        }
    }
}

impl Instruction for VarAssign {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }
}
