//! The Loop instruction is used for repeating instructions. They can be of three
//! different kinds, `for`, `while` or `loop`.

use super::{Block, InstrKind, Instruction, Var};

/// What kind of loop the loop block represents: Either a for Loop, with a variable and
/// a range expression, a while loop with just an upper bound, or a loop with no bound
/// at all
pub enum LoopKind {
    For(Var, Box<dyn Instruction>),
    While(Box<dyn Instruction>),
    Loop,
}

/// The Loop block struct. Contains the block to execute, as well as the kind of loop
/// it represents.
pub struct Loop {
    kind: LoopKind,
    block: Block,
}

impl Loop {
    pub fn new(kind: LoopKind, block: Block) -> Loop {
        Loop { kind, block }
    }
}

impl Instruction for Loop {
    fn kind(&self) -> InstrKind {
        self.block.kind()
    }

    fn print(&self) -> String {
        match &self.kind {
            LoopKind::For(var, range) => format!(
                "for {} in {} {}",
                var.name(),
                range.print(),
                self.block.print()
            ),
            LoopKind::While(condition) => {
                format!("while {} {}", condition.print(), self.block.print())
            }
            LoopKind::Loop => format!("loop {}", self.block.print()),
        }
    }
}
