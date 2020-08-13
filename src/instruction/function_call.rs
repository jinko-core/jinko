//! FunctionCalls are used when calling a function. The argument lists is given to the
//! function on execution.

use crate::value::Constant;

use super::{Instruction, InstrKind};

pub struct FunctionCall {
    /// Name of the function to call
    fn_name: String,

    /// Arguments to give to the function
    args: Vec<Constant>,
}

impl FunctionCall {
    /// Create a new function call and return it
    pub fn new(fn_name: String) -> FunctionCall {
        FunctionCall {
            fn_name,
            args: Vec::new(),
        }
    }

    /// Add an argument to the given function call
    pub fn add_arg(&mut self, arg: Constant) {
        self.args.push(arg)
    }

    /// Return a reference the called function's name
    pub fn name(&self) -> &str {
        &self.fn_name
    }

    /// Return a reference to the list of arguments
    pub fn args(&self) -> &Vec<Constant> {
        &self.args
    }
}

impl Instruction for FunctionCall {
    fn kind(&self) -> InstrKind {
        // FIXME: Add logic
        InstrKind::Expression
    }
}
