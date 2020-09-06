//! The Loop instruction is used for repeating instructions. They can be of three
//! different kinds, `for`, `while` or `loop`.

use super::{Block, Instruction, Var};

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
        Loop {
            kind,
            block,
        }
    }
}
