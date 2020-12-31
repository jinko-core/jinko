//! FunctionCalls are used when calling a function. The argument list is given to the
//! function on execution.

use super::{FunctionDec, InstrKind, Instruction, Var, VarAssign};
use crate::error::{ErrKind, JinkoError};
use crate::interpreter::Interpreter;
use std::rc::Rc;

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

    /// Get the corresponding declaration from an interpreter
    fn get_declaration(
        &self,
        interpreter: &mut Interpreter,
    ) -> Result<Rc<FunctionDec>, JinkoError> {
        match interpreter.get_function(self.name()) {
            // get_function() return a Rc, so this clones the Rc, not the FunctionDec
            Some(f) => Ok(f.clone()),
            // FIXME: Fix Location and input
            None => Err(JinkoError::new(
                ErrKind::Interpreter,
                format!("Cannot find function {}", self.name()),
                None,
                self.name().to_owned(),
            )),
        }
    }

    /// Check if the arguments received and the arguments expected match
    fn check_args_count(&self, function: &FunctionDec) -> Result<(), JinkoError> {
        match self.args().len() == function.args().len() {
            true => Ok(()),
            false => Err(JinkoError::new(
                ErrKind::Interpreter,
                format!(
                    "Wrong number of arguments \
                    for call to function `{}`: Expected {}, got {}",
                    self.name(),
                    function.args().len(),
                    self.args().len()
                ),
                None,
                "".to_owned(),
                // FIXME: Add input and location
            )),
        }
    }

    /// Map each argument to its corresponding instruction
    fn map_args(
        &self,
        function: &FunctionDec,
        interpreter: &mut Interpreter,
    ) -> Result<(), JinkoError> {
        for (call_arg, func_arg) in self.args.iter().zip(function.args()) {
            interpreter.debug(
                "VAR MAP",
                format!("Mapping `{}` to `{}`", func_arg.name(), call_arg.print()).as_ref(),
            );

            interpreter.add_variable(Var::new(func_arg.name().to_owned()))?;

            let var = VarAssign::new(
                false, // FIXME: Do not always mark the variable as immutable
                func_arg.name().to_owned(),
                call_arg.clone(), // TODO: Remove clone?
            );

            var.execute(interpreter)?;
        }

        Ok(())
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
        let function = self.get_declaration(interpreter)?;

        self.check_args_count(&function)?;

        interpreter.debug("CALL", self.name());
        interpreter.scope_enter();

        self.map_args(&function, interpreter)?;

        interpreter.scope_exit();

        function.run(interpreter)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_pretty_print_empty() {
        let function = FunctionCall::new("something".to_owned());

        assert_eq!(function.print(), "something()");
    }

    #[test]
    #[ignore]
    fn t_pretty_print_simple() {
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

    // Don't ignore once variable execution is implemented

    #[test]
    #[ignore]
    fn t_invalid_args_number() {
        use super::super::{FunctionDec, FunctionDecArg};
        use crate::value::JinkInt;

        let mut interpreter = Interpreter::new();

        // Create a new function with two integers arguments
        let mut f = FunctionDec::new("func0".to_owned(), None);
        f.set_args(vec![
            FunctionDecArg::new("a".to_owned(), "int".to_owned()),
            FunctionDecArg::new("b".to_owned(), "int".to_owned()),
        ]);

        interpreter.add_function(f).unwrap();

        let mut f_call = FunctionCall::new("func0".to_string());

        match f_call.execute(&mut interpreter) {
            Ok(_) => assert!(false, "Given 0 arguments to 2 arguments function"),
            Err(_) => assert!(true),
        }

        f_call.add_arg(box JinkInt::from(12));

        match f_call.execute(&mut interpreter) {
            Ok(_) => assert!(false, "Given 1 arguments to 2 arguments function"),
            Err(_) => assert!(true),
        }

        f_call.add_arg(box JinkInt::from(24));

        match f_call.execute(&mut interpreter) {
            Ok(_) => assert!(true),
            Err(e) => assert!(false, "{}: Given 2 arguments to 2 arguments function", e),
        }
    }
}
