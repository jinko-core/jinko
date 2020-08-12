//! Statements are instructions that return `Nothing` upon execution

use super::{Instruction, InstrType, ReturnKind};

#[derive(Clone, Copy)]
pub struct Statement;

// FIXME: Careful of error handling ? What happens when a statement actually returns
// something ?

/// Statements always return `Nothing`
impl Instruction for Statement {
    fn execute(&self) -> Result<ReturnKind, String> {
        Ok(ReturnKind::Nothing)
    }

    fn kind(&self) -> InstrType {
        InstrType::Stmt
    }
}
