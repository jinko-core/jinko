//! Functions are simply a vector of instructions. They execute instructions sequentially,
//! in the order that they were parsed. You can only add instructions and run through
//! the function

use super::instruction::Instruction;

pub struct Function {
    instructions: Vec<Instruction>,
}

impl Function {
    /// Create a new function
    pub fn new() -> Function {
        Function {
            instructions: Vec::new(),
        }
    }

    /// Add a new instruction to a function
    pub fn add(&mut self, instr: Instruction) {
        self.instructions.push(instr)
    }

    /// "Call" the function and run its code
    pub fn call(&self) {
        todo!();
    }
}
