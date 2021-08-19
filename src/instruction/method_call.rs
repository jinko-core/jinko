//! A method like call is syntactic sugar over regular function calls. During executions,
//! they get desugared into a normal function call.

use crate::instruction::FunctionCall;
use crate::{InstrKind, Instruction, Interpreter, Rename, ObjectInstance};

#[derive(Clone)]
pub struct MethodCall {
    var: Box<dyn Instruction>,
    method: FunctionCall,
}

impl MethodCall {
    /// Create a new MethodCall from a variable and an associated function
    pub fn new(var: Box<dyn Instruction>, method: FunctionCall) -> MethodCall {
        MethodCall { var, method }
    }
}

impl Instruction for MethodCall {
    fn kind(&self) -> InstrKind {
        // FIXME: Add logic once typechecking is implemented and we can check for void
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!("{}.{}", self.var.print(), self.method.print())
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Option<ObjectInstance> {
        interpreter.debug("METHOD CALL ENTER", &self.print());

        // FIXME: No clone here
        let mut call = self.method.clone();

        call.add_arg_front(self.var.clone());

        interpreter.debug("DESUGARING TO", &call.print());

        interpreter.debug("METHOD CALL EXIT", &self.print());

        call.execute(interpreter)
    }
}

impl Rename for MethodCall {
    fn prefix(&mut self, prefix: &str) {
        self.var.prefix(prefix);
        self.method.prefix(prefix);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Construct;
    use crate::*;

    #[test]
    fn t_print() {
        let var = Box::new(JkInt::from(15));
        let method = FunctionCall::new("some_int_func".to_owned());
        let mc = MethodCall::new(var, method);

        assert_eq!(mc.print(), "15.some_int_func()".to_owned())
    }

    #[test]
    fn t_execute() {
        let mut interpreter = Interpreter::new();
        let func_dec = Construct::instruction("func first(a: int, b: int) -> int { a }")
            .unwrap()
            .1;
        func_dec.execute(&mut interpreter).unwrap();

        let var1 = Box::new(JkInt::from(1));
        let var2 = Box::new(JkInt::from(2));
        let mut method = FunctionCall::new("first".to_owned());
        method.add_arg(var2);

        let mc = MethodCall::new(var1, method);

        assert_eq!(
            mc.execute(&mut interpreter).unwrap(),
            InstrKind::Expression(Some(JkInt::from(1).to_instance()))
        );
    }
}
