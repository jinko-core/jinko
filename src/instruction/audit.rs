//! Audit blocks are more permissive than normal blocks. They allow ignoring a return
//! value, for example.

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
}
