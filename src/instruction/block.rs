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
//! Otherwise, it's `void`.
//!
//! Blocks are an aggregation site.

use crate::context::Context;
use crate::error::Error;
use crate::generics::{GenericMap, GenericUser};
use crate::instance::ObjectInstance;
use crate::instruction::{InstrKind, Instruction};
use crate::location::SpanTuple;
use crate::log;
use crate::typechecker::{CheckedType, TypeCheck, TypeCtx};

#[derive(Clone, Default)]
pub struct Block {
    instructions: Vec<Box<dyn Instruction>>,
    is_statement: bool,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl Block {
    /// Create a new block
    pub fn new() -> Block {
        // FIXME: Remove this method
        Block {
            instructions: Vec::new(),
            is_statement: true,
            cached_type: None,
            location: None,
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

    /// Set block's location
    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
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
        let mut base = String::from("{\n");

        if let Some((last, instructions)) = self.instructions.split_last() {
            instructions.iter().for_each(|instr| {
                base = format!("{}    {};\n", base, &instr.print());
            });
            base = format!("{}    {}\n", base, last.print());
        }

        base.push('}');
        base
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        ctx.scope_enter();
        log!("block enter");

        let ret_val = self
            .instructions
            .iter()
            .map(|inst| inst.execute(ctx))
            .last();

        ctx.scope_exit();
        log!("block exit");

        match self.is_statement {
            false => ret_val.flatten(),
            true => None,
        }
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for Block {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> Result<CheckedType, Error> {
        let last_type = self
            .instructions
            .iter_mut()
            .map(|inst| match inst.type_of(ctx) {
                // Aggregate
                Err(e) => {
                    ctx.error(e);
                    CheckedType::Error
                }
                Ok(t) => t,
            })
            .last()
            .unwrap_or(CheckedType::Void);

        match &self.is_statement {
            true => Ok(CheckedType::Void),
            false => Ok(last_type),
        }
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl GenericUser for Block {
    fn resolve_usages(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) -> Result<(), Error> {
        // Aggregate errors
        self.instructions.iter_mut().for_each(|inst| {
            if let Err(e) = inst.resolve_usages(type_map, ctx) {
                ctx.error(e)
            }
        });

        Ok(())
    }
}

// TODO: Add tests once TypeCheck is a trait bound on Instruction
#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::{Var, VarAssign};
    use crate::value::JkInt;
    use crate::{jinko, jinko_fail, span};

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
        let mut b = crate::parser::constructs::expr(span!("{ 12; 15; a = 14; }"))
            .unwrap()
            .1;

        let mut ctx = Context::new();

        assert_eq!(ctx.type_check(b.as_mut()).unwrap(), CheckedType::Void)
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
