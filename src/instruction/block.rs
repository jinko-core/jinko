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

use crate::{
    typechecker::{CheckedType, TypeCtx},
    Context, InstrKind, Instruction, ObjectInstance, TypeCheck,
};

#[derive(Clone, Default)]
pub struct Block {
    instructions: Vec<Box<dyn Instruction>>,
    is_statement: bool,
    ty: CheckedType,
}

impl Block {
    /// Create a new block
    pub fn new() -> Block {
        // FIXME: Remove this method
        Block {
            instructions: Vec::new(),
            is_statement: true,
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

    /// Adds a set of instructions to the instructions contained in the block
    pub fn add_instructions(&mut self, instructions: Vec<Box<dyn Instruction>>) {
        self.instructions.extend(instructions)
    }

    /// Add an instruction at the end of the block's instructions
    pub fn add_instruction(&mut self, instruction: Box<dyn Instruction>) {
        self.instructions.push(instruction);
    }

    /// Add an instruction at the start of the block's instructions
    pub fn push_front_instruction(&mut self, instruction: Box<dyn Instruction>) {
        self.instructions.insert(0, instruction);
    }

    /// Pop an instruction from the block, removing it from the execution pool
    pub fn pop_instruction(&mut self) -> Option<Box<dyn Instruction>> {
        self.instructions.pop()
    }

    /// Set block is_statement to given value
    pub fn set_statement(&mut self, is_statement: bool) {
        self.is_statement = is_statement;
    }
}

impl Instruction for Block {
    fn kind(&self) -> InstrKind {
        match self.is_statement {
            true => InstrKind::Statement,
            false => self.instructions.last().unwrap().kind(),
        }
    }

    fn print(&self) -> String {
        if self.instructions.is_empty() {
            "{\n}".to_owned()
        } else {
            format!(
                "{{\n{}\n}}",
                self.instructions()
                    .iter()
                    .map(|inst| inst.print())
                    .intersperse("\n;\n    ".to_owned())
                    .collect::<String>()
            )
        }
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        ctx.scope_enter();
        ctx.debug_step("BLOCK ENTER");

        let ret_val = self
            .instructions
            .iter()
            .map(|inst| inst.execute(ctx))
            .last();

        ctx.scope_exit();
        ctx.debug_step("BLOCK EXIT");

        match self.is_statement {
            false => ret_val.flatten(),
            true => None,
        }
    }
}

impl TypeCheck for Block {
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType {
        let last_type = self
            .instructions
            .iter()
            .map(|inst| inst.resolve_type(ctx))
            .last()
            .unwrap_or(CheckedType::Void);

        match &self.is_statement {
            true => CheckedType::Void,
            false => last_type,
        }
    }
}

// TODO: Add tests once TypeCheck is a trait bound on Instruction
#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::{Var, VarAssign};
    use crate::value::JkInt;
    use crate::TypeCheck;
    use crate::{jinko, jinko_fail};

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
        b.set_statement(false);

        assert_eq!(b.kind(), InstrKind::Expression(None));
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
        b.add_instruction(last);
        b.set_statement(false);

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

        let instr: Vec<Box<dyn Instruction>> = vec![
            Box::new(JkInt::from(12)),
            Box::new(JkInt::from(15)),
            Box::new(VarAssign::new(
                true,
                String::from("a"),
                Box::new(JkInt::from(15)),
            )),
        ];
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
        b.add_instruction(last);
        b.set_statement(false);

        let mut i = Context::new();

        assert_eq!(b.execute(&mut i).unwrap(), JkInt::from(18).to_instance());
        assert!(!i.error_handler.has_errors());
    }

    // FIXME: Add test for type of block containing `last` once TypeChecker is implemented
    // for all Instructions
    // FIXME: Do not ignore once #337 is fixed
    #[test]
    #[ignore]
    fn block_no_last_tychk() {
        let b = crate::parser::constructs::expr("{ 12; 15; a = 14; }")
            .unwrap()
            .1;

        let mut ctx = Context::new();
        let mut ctx = TypeCtx::new(&mut ctx);

        assert_eq!(b.resolve_type(&mut ctx), CheckedType::Void)
    }

    #[test]
    fn tc_block_valid() {
        jinko! {
            func takes_int(i: int) {}
            takes_int({ 15 });
            takes_int({ { { 14 } } });
        };
    }

    #[test]
    fn tc_block_invalid() {
        jinko_fail! {
            func takes_int(i: int) {}
            takes_int({ 0.4 });
            takes_int({ { { true } } });
        };
    }
}
