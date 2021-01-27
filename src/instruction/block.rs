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
use crate::{error::JkError, interpreter::Interpreter};

#[derive(Clone)]
pub struct Block {
    instructions: Vec<Box<dyn Instruction>>,
    last: Option<Box<dyn Instruction>>,
}

impl Block {
    /// Create a new function
    pub fn new() -> Block {
        Block {
            instructions: Vec::new(),
            last: None,
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

    /// Add an instruction at the end of the block's instructions
    pub fn add_instruction(&mut self, instruction: Box<dyn Instruction>) {
        self.instructions.push(instruction)
    }

    /// Pop an instruction from the block, removing it from the execution pool
    pub fn pop_instruction(&mut self) -> Option<Box<dyn Instruction>> {
        self.instructions.pop()
    }

    /// Returns a reference to the last expression of the block, if it exists
    pub fn last(&self) -> Option<&Box<dyn Instruction>> {
        self.last.as_ref()
    }

    /// Gives a last expression to the block
    pub fn set_last(&mut self, last: Option<Box<dyn Instruction>>) {
        self.last = last;
    }
}

impl Instruction for Block {
    fn kind(&self) -> InstrKind {
        match self.last() {
            Some(last) => last.as_ref().kind(),
            None => InstrKind::Statement,
        }
    }

    fn print(&self) -> String {
        let mut base = String::from("{\n");

        for instr in &self
            .instructions
            .iter()
            .collect::<Vec<&Box<dyn Instruction>>>()
        {
            base = format!("{}    {}", base, &instr.print());
            base.push_str(";\n");
        }

        match self.last() {
            Some(l) => base = format!("{}    {}\n", base, l.print()),
            None => {}
        }

        base.push_str("}");
        base
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.scope_enter();
        interpreter.debug_step("BLOCK ENTER");

        self.instructions()
            .iter()
            .map(|inst| inst.execute(interpreter))
            .collect::<Result<Vec<InstrKind>, JkError>>()?;

        let ret_val = match &self.last {
            Some(e) => e.execute(interpreter),
            None => Ok(InstrKind::Statement),
        };

        interpreter.scope_exit();
        interpreter.debug_step("BLOCK EXIT");

        ret_val
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Var;
    use crate::value::JinkInt;

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
    }

    #[test]
    fn stmt_plus_expr() {
        let mut b = Block::new();
        let instrs: Vec<Box<dyn Instruction>> = vec![
            Box::new(Var::new("x".to_owned())),
            Box::new(Var::new("n".to_owned())),
            Box::new(JinkInt::from(14)),
        ];
        let last = Box::new(JinkInt::from(12));

        b.set_instructions(instrs);
        b.set_last(Some(last));

        assert_eq!(b.kind(), InstrKind::Expression(None));
    }

    #[test]
    fn block_execute_empty() {
        let b = Block::new();

        let mut i = Interpreter::new();

        assert_eq!(b.execute(&mut i).unwrap(), InstrKind::Statement);
    }

    #[test]
    fn block_execute_no_last() {
        let mut b = Block::new();

        let instr: Vec<Box<dyn Instruction>> =
            vec![Box::new(JinkInt::from(12)), Box::new(JinkInt::from(15))];
        b.set_instructions(instr);

        let mut i = Interpreter::new();

        assert_eq!(b.execute(&mut i).unwrap(), InstrKind::Statement);
    }

    #[test]
    fn block_execute_with_last() {
        use crate::instance::ToInstance;

        let mut b = Block::new();

        let instr: Vec<Box<dyn Instruction>> =
            vec![Box::new(JinkInt::from(12)), Box::new(JinkInt::from(15))];
        b.set_instructions(instr);

        let last = Box::new(JinkInt::from(18));
        b.set_last(Some(last));

        let mut i = Interpreter::new();

        assert_eq!(
            b.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JinkInt::from(18).to_instance()))
        );
    }
}
