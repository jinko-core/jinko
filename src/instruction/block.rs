//! Blocks are simply a vector of instructions. They execute instructions sequentially,
//! in the order that they were parsed. You can only add instructions and run through
//! the block.
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

use crate::{typechecker::CheckedType, Context, InstrKind, Instruction, ObjectInstance, TypeCheck};

#[derive(Clone, Default)]
pub struct Block {
    instructions: Vec<Box<dyn Instruction>>,
    last: Option<Box<dyn Instruction>>,
    ty: CheckedType,
}

impl Block {
    /// Create a new block
    pub fn new() -> Block {
        // FIXME: Remove this method
        Block {
            instructions: Vec::new(),
            last: None,
            ty: CheckedType::Unknown,
        }
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
    pub fn last(&self) -> Option<&dyn Instruction> {
        self.last.as_deref()
    }

    /// Gives a last expression to the block
    pub fn set_last(&mut self, last: Option<Box<dyn Instruction>>) {
        self.last = last;
    }
}

impl Instruction for Block {
    fn kind(&self) -> InstrKind {
        match self.last() {
            Some(last) => last.kind(),
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

        if let Some(l) = self.last() {
            base = format!("{}    {}\n", base, l.print());
        }

        base.push('}');
        base
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        ctx.scope_enter();
        ctx.debug_step("BLOCK ENTER");

        self.instructions().iter().for_each(|inst| {
            inst.execute(ctx);
        });

        let ret_val = match &self.last {
            Some(e) => e.execute(ctx),
            None => None,
        };

        ctx.scope_exit();
        ctx.debug_step("BLOCK EXIT");

        ret_val
    }
}

impl TypeCheck for Block {
    fn resolve_type(&self, _ctx: &mut Context) -> CheckedType {
        match &self.last {
            None => CheckedType::Void,
            Some(_last) => CheckedType::Unknown, // FIXME: last.resolve_type(ctx),
        }
    }
}

// TODO: Add tests once TypeCheck is a trait bound on Instruction
#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Var;
    use crate::value::JkInt;
    use crate::TypeCheck;

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
            Box::new(JkInt::from(14)),
        ];
        let last = Box::new(JkInt::from(12));

        b.set_instructions(instrs);
        b.set_last(Some(last));

        assert_eq!(b.kind(), InstrKind::Expression(None));
    }

    #[test]
    fn block_execute_empty() {
        let b = Block::new();

        let mut i = Context::new();

        assert_eq!(b.execute(&mut i), None);
        assert!(!i.error_handler.has_errors());
    }

    #[test]
    fn block_execute_no_last() {
        let mut b = Block::new();

        let instr: Vec<Box<dyn Instruction>> =
            vec![Box::new(JkInt::from(12)), Box::new(JkInt::from(15))];
        b.set_instructions(instr);

        let mut i = Context::new();

        assert_eq!(b.execute(&mut i), None);
        assert!(!i.error_handler.has_errors());
    }

    #[test]
    fn block_execute_with_last() {
        use crate::instance::ToObjectInstance;

        let mut b = Block::new();

        let instr: Vec<Box<dyn Instruction>> =
            vec![Box::new(JkInt::from(12)), Box::new(JkInt::from(15))];
        b.set_instructions(instr);

        let last = Box::new(JkInt::from(18));
        b.set_last(Some(last));

        let mut i = Context::new();

        assert_eq!(b.execute(&mut i).unwrap(), JkInt::from(18).to_instance());
        assert!(!i.error_handler.has_errors());
    }

    // FIXME: Add test for type of block containing `last` once TypeChecker is implemented
    // for all Instructions
    #[test]
    fn block_no_last_tychk() {
        let mut b = Block::new();
        let instr: Vec<Box<dyn Instruction>> =
            vec![Box::new(JkInt::from(12)), Box::new(JkInt::from(15))];
        b.set_instructions(instr);

        let mut ctx = Context::new();

        assert_eq!(b.resolve_type(&mut ctx), CheckedType::Void)
    }
}
