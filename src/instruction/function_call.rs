//! FunctionCalls are used when calling a function. The argument lists is given to the
//! function on execution.

use super::{InstrKind, Instruction};
use crate::error::{ErrKind, JinkoError};
use crate::interpreter::Interpreter;

#[derive(Clone)]
pub struct FunctionCall {
    /// Name of the function to call
    fn_name: String,

    /// Arguments to give to the function
    args: Vec<Box<dyn Instruction>>,
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
    pub fn add_arg(&mut self, arg: Box<dyn Instruction>) {
        self.args.push(arg)
    }

    /// Return a reference the called function's name
    pub fn name(&self) -> &str {
        &self.fn_name
    }

    /// Return a reference to the list of arguments
    pub fn args(&self) -> &Vec<Box<dyn Instruction>> {
        &self.args
    }
}

impl Instruction for FunctionCall {
    fn kind(&self) -> InstrKind {
        // FIXME: Add logic
        InstrKind::Expression
    }

    fn print(&self) -> String {
        let mut base = format!("{}(", self.fn_name);

        let mut first_arg = true;
        for arg in &self.args {
            if !first_arg {
                base.push_str(", ");
            }

            base.push_str(&arg.print());

            first_arg = false;
        }

        format!("{})", base)
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<(), JinkoError> {
        let function = match interpreter.get_function(self.name()) {
            // get_function() return a Rc, so this clones the Rc, not the FunctionDec
            Some(f) => f.clone(),
            // FIXME: Fix Location and input
            None => {
                return Err(JinkoError::new(
                    ErrKind::Interpreter,
                    format!("cannot find function {}", self.name()),
                    None,
                    self.name().to_owned(),
                ))
            }
        };

        interpreter.debug("CALL", self.name());

        function.run(interpreter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pretty_print_empty() {
        let function = FunctionCall::new("something".to_owned());

        assert_eq!(function.print(), "something()");
    }

    #[test]
    #[ignore]
    fn pretty_print_simple() {
        /*
        let c0 = Constant::new(ConstKind::Int).with_iv(12);
        let c1 = Constant::new(ConstKind::Int).with_iv(13);
        let c2 = Constant::new(ConstKind::Int).with_iv(14);

        let mut function = FunctionCall::new("fn_name".to_string());

        function.add_arg(c0);
        function.add_arg(c1);
        function.add_arg(c2);

        assert_eq!(function.print(), "fn_name(12, 13, 14)");
        */
    }
}
