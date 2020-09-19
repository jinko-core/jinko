//! Blocks are simply a vector of instructions. They execute instructions sequentially,
//! in the order that they were parsed. You can only add instructions and run through
//! the block. They contain a return value, that you can access once the block is done
//! running.
//! Blocks are used to represent scope blocks
//!
//! `{ something(); something_else(); }`
//!
//! function definitions (as well as test and mock definitions)
//!
//! `func something() { do_something_else() }`
//!
//! and other kind of code blocks, for example in if/else blocks or loops
//!
//! `if something() { do_something(); }`
//! `loop { do_something_repeatedly(); }`
//!
//! The return value of the function is the last instruction if it is an expression.
//! Otherwise, it's `void`

use super::{InstrKind, Instruction};
use crate::{error::JinkoError, interpreter::Interpreter};

pub struct Block {
    instructions: Vec<Box<dyn Instruction>>,
}

impl Block {
    /// Create a new function
    pub fn new() -> Block {
        Block {
            instructions: Vec::new(),
        }
    }

    /// "Call" the function and run its code
    pub fn call(&self) {
        todo!();
    }

    /// Returns a reference to the instructions contained in the block
    pub fn instructions(&self) -> &Vec<Box<dyn Instruction>> {
        &self.instructions
    }

    /// Gives a set of instructions to the block
    pub fn set_instructions(&mut self, instructions: Vec<Box<dyn Instruction>>) {
        self.instructions = instructions;
    }

    /// Add an instruction for the block to execute
    pub fn add_instruction(&mut self, instruction: Box<dyn Instruction>) {
        self.instructions.push(instruction)
    }
}

impl Instruction for Block {
    fn kind(&self) -> InstrKind {
        match self.instructions.last() {
            Some(last) => last.as_ref().kind(),
            None => InstrKind::Statement,
        }
    }

    fn print(&self) -> String {
        let mut base = String::from("{\n");

        for instr in &self.instructions {
            base = format!("{}    {}", base, &instr.print());
            base.push_str(match instr.kind() {
                InstrKind::Statement => ";\n",
                InstrKind::Expression | InstrKind::FuncDec => "\n",
            });
        }

        base.push_str("}");
        base
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), JinkoError> {
        interpreter.scope_enter();

        self.instructions()
            .iter()
            .map(|inst| inst.execute(interpreter))
            .collect::<Result<Vec<()>, JinkoError>>()?;

        interpreter.scope_exit();

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Var;
    use crate::value::constant::{ConstKind, Constant};

    #[test]
    fn empty() {
        let b = Block::new();

        assert_eq!(b.kind(), InstrKind::Statement);
        assert_eq!(b.print(), "{\n}");
    }

    #[test]
    fn all_stmts() {
        let mut b = Block::new();
        let instrs: Vec<Box<dyn Instruction>> = vec![
            Box::new(Var::new("x".to_owned())),
            Box::new(Var::new("n".to_owned())),
        ];

        b.set_instructions(instrs);

        assert_eq!(b.kind(), InstrKind::Statement);
        assert_eq!(
            b.print(),
            r#"{
    x;
    n;
}"#
        );
    }

    #[test]
    fn stmt_plus_expr() {
        let mut b = Block::new();
        let instrs: Vec<Box<dyn Instruction>> = vec![
            Box::new(Var::new("x".to_owned())),
            Box::new(Var::new("n".to_owned())),
            Box::new(Constant::new(ConstKind::Int).with_iv(14)),
        ];

        b.set_instructions(instrs);

        assert_eq!(b.kind(), InstrKind::Expression);
        assert_eq!(
            b.print(),
            r#"{
    x;
    n;
    14
}"#
        );
    }
}
