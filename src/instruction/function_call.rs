//! FunctionCalls are used when calling a function. The argument list is given to the
//! function on execution.

use crate::instruction::{FunctionDec, Var};
use crate::{ErrKind, Error, InstrKind, Instruction, Interpreter, Rename, ObjectInstance};
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

    /// Add an argument to the beginning of the function call's argument list. This is
    /// only useful for method call desugaring
    pub fn add_arg_front(&mut self, arg: Box<dyn Instruction>) {
        self.args.insert(0, arg)
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
    fn get_declaration(&self, interpreter: &mut Interpreter) -> Result<Rc<FunctionDec>, Error> {
        match interpreter.get_function(self.name()) {
            // get_function() return a Rc, so this clones the Rc, not the FunctionDec
            Some(f) => Ok(f.clone()),
            // FIXME: Fix Location and input
            None => Err(Error::new(ErrKind::Interpreter)
                .with_msg(format!("cannot find function {}", self.name()))),
        }
    }

    /// Check if the arguments received and the arguments expected match
    fn check_args_count(&self, function: &FunctionDec) -> Result<(), Error> {
        match self.args().len() == function.args().len() {
            true => Ok(()),
            false => Err(Error::new(ErrKind::Interpreter).with_msg(format!(
                "wrong number of arguments \
                    for call to function `{}`: expected {}, got {}",
                self.name(),
                function.args().len(),
                self.args().len()
            ))),
        }
    }

    /// Map each argument to its corresponding instruction
    fn map_args(&self, function: &FunctionDec, interpreter: &mut Interpreter) {
        for (call_arg, func_arg) in self.args.iter().zip(function.args()) {
            interpreter.debug(
                "VAR MAP",
                format!("Mapping `{}` to `{}`", func_arg.name(), call_arg.print()).as_ref(),
            );

            // Create a new variable, and execute the content of the function argument
            // passed to the call
            let mut new_var = Var::new(func_arg.name().to_owned());
            let mut instance = match call_arg.execute_expression(interpreter) {
                Some(i) => i,
                None => return,
            };

            let ty = match interpreter.get_type(func_arg.get_type()) {
                // Double dereferencing: Some(t) gives us a &Rc<TypeDec>. We dereference
                // it to access the Rc, and dereference it again to access the TypeDec.
                Some(t) => (**t).clone(),
                None => {
                    interpreter.error(Error::new(ErrKind::Interpreter)
                        .with_msg(format!("type not found: {}", func_arg.get_type().id())));
                        return
                }
            };

            instance.set_ty(Some(ty));

            new_var.set_instance(instance);

            if let Err(e) = interpreter.add_variable(new_var) {
                interpreter.error(e);
            }
        }
    }
}

impl Instruction for FunctionCall {
    fn kind(&self) -> InstrKind {
        // FIXME: Add logic
        InstrKind::Expression(None)
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

    fn execute(&self, interpreter: &mut Interpreter) -> Option<ObjectInstance> {
        let function = match self.get_declaration(interpreter) {
            Ok(f) => f,
            Err(e) => { interpreter.error(e); return None; }
        };

        if let Err(e) = self.check_args_count(&function) {
            interpreter.error(e); return None;
        }

        interpreter.scope_enter();

        interpreter.debug("CALL", self.name());

        self.map_args(&function, interpreter);

        let ret_val = function.run(interpreter);

        interpreter.scope_exit();

        ret_val
    }
}

impl Rename for FunctionCall {
    fn prefix(&mut self, prefix: &str) {
        self.fn_name = format!("{}{}", prefix, self.fn_name);

        self.args.iter_mut().for_each(|arg| arg.prefix(prefix));
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::TypeId;

    #[test]
    fn t_pretty_print_empty() {
        let function = FunctionCall::new("something".to_owned());

        assert_eq!(function.print(), "something()");
    }

    // Don't ignore once variable execution is implemented

    #[test]
    fn t_invalid_args_number() {
        use super::super::{DecArg, FunctionDec};
        use crate::instruction::FunctionKind;
        use crate::value::JkInt;

        let mut interpreter = Interpreter::new();

        // Create a new function with two integers arguments
        let mut f = FunctionDec::new("func0".to_owned(), None);
        f.set_kind(FunctionKind::Func);

        f.set_args(vec![
            DecArg::new("a".to_owned(), TypeId::from("int")),
            DecArg::new("b".to_owned(), TypeId::from("int")),
        ]);

        interpreter.add_function(f).unwrap();

        let mut f_call = FunctionCall::new("func0".to_string());

        match f_call.execute(&mut interpreter) {
            Ok(_) => assert!(false, "Given 0 arguments to 2 arguments function"),
            Err(_) => assert!(true),
        }

        f_call.add_arg(Box::new(JkInt::from(12)));

        match f_call.execute(&mut interpreter) {
            Ok(_) => assert!(false, "Given 1 arguments to 2 arguments function"),
            Err(_) => assert!(true),
        }
    }

    #[test]
    fn t_func_call_arg_return() {
        use crate::parser::Construct;
        use crate::value::JkInt;
        use crate::ToObjectInstance;

        let mut i = Interpreter::new();
        let func_dec = Construct::instruction("func second(f: int, s: int) -> int { s }")
            .unwrap()
            .1;
        let func_call = Construct::instruction("second(1, 2)").unwrap().1;

        func_dec.execute(&mut i).unwrap();

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(2).to_instance()))
        );
    }

    #[test]
    fn t_func_call_arg_return_binop() {
        use crate::parser::Construct;
        use crate::value::JkInt;
        use crate::ToObjectInstance;

        let mut i = Interpreter::new();
        let func_dec = Construct::instruction("func add(a: int, b: int) -> int { a + b }")
            .unwrap()
            .1;
        let func_call = Construct::instruction("add(1, 2)").unwrap().1;

        func_dec.execute(&mut i).unwrap();

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(3).to_instance()))
        );
    }

    #[test]
    fn t_func_call_variable_return() {
        use crate::parser::Construct;
        use crate::value::JkInt;
        use crate::ToObjectInstance;

        let mut i = Interpreter::new();
        let func_dec = Construct::instruction("func one() -> int { one = 1; one }")
            .unwrap()
            .1;
        let func_call = Construct::instruction("one()").unwrap().1;

        func_dec.execute(&mut i).unwrap();

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            InstrKind::Expression(Some(JkInt::from(1).to_instance()))
        );
    }
}
