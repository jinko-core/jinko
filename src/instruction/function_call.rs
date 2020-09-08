//! FunctionCalls are used when calling a function. The argument lists is given to the
//! function on execution.

use crate::error::{BroccoliError, ErrKind};
use crate::interpreter::Interpreter;
use crate::value::Constant;

use super::{InstrKind, Instruction};

pub struct FunctionCall {
    /// Name of the function to call
    fn_name: String,

    /// Arguments to give to the function
    args: Vec<Constant>,
    // FIXME: Use Box<dyn Instruction> or something along those lines
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

    fn execute(&mut self, interpreter: &mut Interpreter) -> Result<(), BroccoliError> {
        let function = match interpreter.get_function(self.name()) {
            Some(f) => f,
            // FIXME: Fix Location and input
            None => return Err(BroccoliError::new(ErrKind::Interpreter, format!("cannot find function {}", self.name()), None, self.name().to_owned()))
        };

        match function.block() {
            Some(b) => b.execute(interpreter),
            // FIXME: Fix Location and input
            None => return Err(BroccoliError::new(ErrKind::Interpreter, format!("Cannot execute function with no body: {}", self.name()), None, self.name().to_owned())),
        };

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::constant::*;

    #[test]
    fn pretty_print_empty() {
        let function = FunctionCall::new("something".to_owned());

        assert_eq!(function.print(), "something()");
    }

    #[test]
    fn pretty_print_simple() {
        let c0 = Constant::new(ConstKind::Int).with_iv(12);
        let c1 = Constant::new(ConstKind::Int).with_iv(13);
        let c2 = Constant::new(ConstKind::Int).with_iv(14);

        let mut function = FunctionCall::new("fn_name".to_string());

        function.add_arg(c0);
        function.add_arg(c1);
        function.add_arg(c2);

        assert_eq!(function.print(), "fn_name(12, 13, 14)");
    }
}
