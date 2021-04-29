//! Instructions are used to represent a single expression/statement in jinko.
//! When using nested instructions, such as `foo = bar();`, you're actually using
//! two instructions: A function call expression, and a variable assignment statement

use crate::{Interpreter, JkErrKind, JkError, ObjectInstance};

use colored::Colorize;
use downcast_rs::{impl_downcast, Downcast};

mod binary_op;
mod block;
mod dec_arg;
mod function_call;
mod function_declaration;
mod if_else;
mod incl;
mod jk_inst;
mod loop_block;
mod method_call;
mod operator;
mod rename;
mod type_declaration;
mod type_id;
mod type_instantiation;
mod var;
mod var_assignment;

pub use binary_op::BinaryOp;
pub use block::Block;
pub use dec_arg::DecArg;
pub use function_call::FunctionCall;
pub use function_declaration::{FunctionDec, FunctionKind};
pub use if_else::IfElse;
pub use incl::Incl;
pub use jk_inst::JkInst;
pub use loop_block::{Loop, LoopKind};
pub use method_call::MethodCall;
pub use operator::Operator;
pub use rename::Rename;
pub use type_declaration::TypeDec;
pub use type_id::{TypeId, PRIMITIVE_TYPES};
pub use type_instantiation::TypeInstantiation;
pub use var::Var;
pub use var_assignment::VarAssign;

/// The type of instructions available. An Instruction either is a statement, or an
/// expression. An expression contains an instance of a result. For example,
/// `1 + 1` is an expression: It will contain the result of the addition of one and one.
/// `print("jinko")` is a statement: There is no "return value"
#[derive(Debug, PartialEq, Clone)]
pub enum InstrKind {
    Statement,
    Expression(Option<ObjectInstance>),
}

/// The `Instruction` trait is the basic trait for all of Jinko's execution nodes. Each
/// node that can be executed needs to implement it
pub trait Instruction: InstructionClone + Downcast + Rename {
    /// Execute the instruction, altering the state of the interpreter. Executing
    /// this method returns an InstrKind, so either a statement or an expression
    /// containing a "return value".
    fn execute(&self, _interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        unreachable!(
            "\n{}\n --> {}",
            self.print(),
            "The execution of this instruction is not implemented yet. This is a bug".red(),
        )
    }

    /// Execute the instruction, hoping for an InstrKind::Expression(Some(...)) to be
    /// returned. If an invalid value is returned, error out.
    fn execute_expression(&self, i: &mut Interpreter) -> Result<ObjectInstance, JkError> {
        match self.execute(i)? {
            InstrKind::Expression(Some(result)) => Ok(result),
            _ => Err(JkError::new(
                JkErrKind::Interpreter,
                format!(
                    "statement found when expression was expected: {}",
                    self.print()
                ),
                None,
                self.print(),
            )),
        }
    }

    /// Execute the instruction, hoping for an InstrKind::Statement to be
    /// returned. If an invalid value is returned, error out.
    fn execute_statement(&self, i: &mut Interpreter) -> Result<(), JkError> {
        match self.execute(i)? {
            InstrKind::Statement => Ok(()),
            _ => Err(JkError::new(
                JkErrKind::Interpreter,
                format!(
                    "expression found when statement was expected: {}",
                    self.print()
                ),
                None,
                self.print(),
            )),
        }
    }

    /// Maybe execute the instruction, transforming it in a Rust bool if possible. It's
    /// only possible to execute as_bool on boolean variables, boolean constants. blocks
    /// returning a boolean and functions returning a boolean.
    fn as_bool(&self, _interpreter: &mut Interpreter) -> Result<bool, JkError> {
        Err(JkError::new(
            JkErrKind::Interpreter,
            format!("cannot be used as a boolean: {}", self.print()),
            None,
            self.print(),
        ))
    }

    /// What is the type of the instruction: a Statement or an Expression.
    /// This method will always return Expression(None) if the instruction is an
    /// expression. This method does not care about the return value or the execution
    /// of the instruction, just the kind of it.
    fn kind(&self) -> InstrKind;

    /// Pretty-print the instruction to valid jinko code
    fn print(&self) -> String;
}

impl_downcast!(Instruction);

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
