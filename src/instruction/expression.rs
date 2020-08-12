//! Expressions are instructions that return `Something` upon execution

use crate::value::Value;

use super::{Instruction, InstrType, ReturnKind};

#[derive(Clone, Copy)]
pub struct Expression {
    // ret_val: Box<dyn Value<Contained = T>>,
}

impl Instruction for Expression {
    fn execute(&self) -> Result<ReturnKind, String> {
        // FIXME: Actually return something
        Ok(ReturnKind::Something)
    }

    fn kind(&self) -> InstrType {
        InstrType::Expr
    }
}
