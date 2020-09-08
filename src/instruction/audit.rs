//! Audit blocks are more permissive than normal blocks. They allow ignoring a return
//! value, for example.

use crate::error::BroccoliError;
use crate::interpreter::Interpreter;

use super::{Block, InstrKind, Instruction};

pub struct Audit {
    block: Block,
}

impl Audit {
    /// Create a new assign block
    pub fn new(block: Block) -> Audit {
        Audit { block }
    }
}

impl Instruction for Audit {
    fn kind(&self) -> InstrKind {
        self.block.kind()
    }

    fn print(&self) -> String {
        format!("audit {}", self.block.print())
    }

    fn execute<'i>(&mut self, interpreter: &'i mut Interpreter) -> Result<(), BroccoliError<'i>> {
        interpreter.audit_enter();

        // FIXME: Use block execute as return value
        self.block.execute(interpreter);

        interpreter.audit_exit();

        Ok(())
    }
}
