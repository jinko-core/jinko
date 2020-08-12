//! Blocks are simply a vector of instructions. They execute instructions sequentially,
//! in the order that they were parsed. You can only add instructions and run through
//! the block. They contain a return value, that you can access once the block is done
//! running.
//! Blocks are used to represent scope blocks
//!
//! `{ something(); something_else(); }`
//!
//! and function definitions (as well as test and mock definitions)
//!
//! `func something() { do_something_else() }`
//!
//! The return value of the function is the last instruction if it is an expression.
//! Otherwise, it's `void`

use super::instruction::Instruction;

pub struct Block {
    instructions: Vec<dyn Instruction>,
}

impl Block {
    /// Create a new function
    pub fn new() -> Block {
        Block {
            instructions: Vec::new(),
        }
    }

    /// Add a new instruction to a function
    pub fn add(&mut self, instr: dyn Instruction) {
        self.instructions.push(instr)
    }

    /// "Call" the function and run its code
    pub fn call(&self) {
        todo!();
    }
}
