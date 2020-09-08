//! `IfElse`s are used to represent an if/else statement in the source code. They have
//! a condition, a body and an optional else body.
//!
//! ```
//! if condition {
//!     condition_evaluates_to_true();
//! } else {
//!     all_conditions_are_false();
//! }
//! ```
//!
//! They can be used to return values, just like you would with any block.
//!
//! ```
//! x = if condition { 12 } else { 13 };
//! ```

use crate::error::BroccoliError;
use crate::interpreter::Interpreter;

use super::{Block, InstrKind, Instruction};

pub struct IfElse {
    condition: Box<dyn Instruction>,
    if_body: Block,
    else_body: Option<Block>,
}

impl IfElse {
    /// Create a new IfElse block and return it
    pub fn new(
        condition: Box<dyn Instruction>,
        if_body: Block,
        else_body: Option<Block>,
    ) -> IfElse {
        IfElse {
            condition,
            if_body,
            else_body,
        }
    }
}

impl Instruction for IfElse {
    fn kind(&self) -> InstrKind {
        // We don't check the kind of the else_body, since the typechecker will have
        // approved that the if_body and else_body return the same thing
        self.if_body.kind()
    }

    fn print(&self) -> String {
        let base = format!("if {} {}", self.condition.print(), self.if_body.print());

        match &self.else_body {
            Some(body) => format!(" else {}", body.print()),
            None => base,
        }
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), BroccoliError> {
        let cond = self.condition.as_bool();

        if cond {
            self.if_body.execute(interpreter)
        } else {
            match &self.else_body {
                Some(b) => b.execute(interpreter),
                None => Ok(()),
            }
        }
    }
}

// FIXME: Add printing tests for if else
#[cfg(test)]
mod tests {}
