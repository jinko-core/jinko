//! Instructions are used to represent a single expression/statement in jinko.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

use std::fmt::Debug;

use crate::context::Context;
use crate::generics::GenericUser;
use crate::instance::ObjectInstance;
use crate::location::SpanTuple;
use crate::typechecker::TypeCheck;

use colored::Colorize;
use downcast_rs::{impl_downcast, Downcast};

mod binary_op;
mod block;
mod dec_arg;
mod field_access;
mod function_call;
mod function_declaration;
mod if_else;
mod incl;
mod jk_inst;
mod jk_return;
mod loop_block;
mod method_call;
mod operator;
mod rename;
mod type_declaration;
mod type_instantiation;
mod var;
mod var_assignment;
mod var_or_empty_type;

pub use binary_op::BinaryOp;
pub use block::Block;
pub use dec_arg::DecArg;
pub use field_access::FieldAccess;
pub use function_call::FunctionCall;
pub use function_declaration::{FunctionDec, FunctionKind};
pub use if_else::IfElse;
pub use incl::Incl;
pub use jk_inst::{JkInst, JkInstKind};
pub use jk_return::Return;
pub use loop_block::{Loop, LoopKind};
pub use method_call::MethodCall;
pub use operator::Operator;
pub use type_declaration::TypeDec;
pub use type_instantiation::TypeInstantiation;
pub use var::Var;
pub use var_assignment::VarAssign;
pub use var_or_empty_type::VarOrEmptyType;

/// The type of instructions available. An Instruction either is a statement, or an
/// expression. An expression contains an instance of a result. For example,
/// `1 + 1` is an expression: It will contain the result of the addition of one and one.
/// `print("jinko")` is a statement: There is no "return value"
// FIXME: Make InstrKind a simpler enum
#[derive(Debug, PartialEq, Clone)]
pub enum InstrKind {
    Statement,
    Expression(Option<ObjectInstance>),
}

// FIXME: Fix documentation for execute_*()

/// The `Instruction` trait is the basic trait for all of Jinko's execution nodes. Each
/// node that can be executed needs to implement it
pub trait Instruction: InstructionClone + Downcast + TypeCheck + GenericUser {
    // FIXME: Add Rename here
    /// Execute the instruction, altering the state of the context. Executing
    /// this method may return an object instance
    fn execute(&self, _ctx: &mut Context) -> Option<ObjectInstance> {
        unreachable!(
            "\n{}\n --> {}",
            self.print(),
            "The execution of this instruction is not implemented yet. This is a bug".red(),
        )
    }

    /// Execute the instruction, hoping for an instance to be returned. If no instance is
    /// returned, error out.
    fn execute_expression(&self, ctx: &mut Context) -> ObjectInstance {
        self.execute(ctx).unwrap()
    }

    /// Execute the instruction, hoping for no instance to be returned. If an instance is
    /// returned, error out.
    // FIXME: Cleanup the return type of this function
    fn execute_statement(&self, ctx: &mut Context) {
        // FIXME: Is there a better way to do this?
        if let Some(_) = self.execute(ctx) {
            unreachable!()
        }
    }

    /// What is the type of the instruction: a Statement or an Expression.
    /// This method will always return Expression(None) if the instruction is an
    /// expression. This method does not care about the return value or the execution
    /// of the instruction, just the kind of it.
    fn kind(&self) -> InstrKind;

    /// Pretty-print the instruction to valid jinko code
    fn print(&self) -> String;

    /// Fetch a reference to this instruction's location
    fn location(&self) -> Option<&SpanTuple> {
        // FIXME: Remove default implementation
        None
    }
}

impl_downcast!(Instruction);

impl Debug for dyn Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

/// The `InstructionClone` provides a wrapper around `Instruction` to allow cloning them
pub trait InstructionClone {
    fn box_clone(&self) -> Box<dyn Instruction>;
}

impl<T> InstructionClone for T
where
    T: 'static + Instruction + Clone,
{
    fn box_clone(&self) -> Box<dyn Instruction> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Instruction> {
    fn clone(&self) -> Self {
        self.box_clone()
    }
}
