//! A method like call is syntactic sugar over regular function calls. During executions,
//! they get desugared into a normal function call.

use crate::instruction::FunctionCall;
use crate::typechecker::{CheckedType, TypeCtx};
use crate::{Context, InstrKind, Instruction, ObjectInstance, TypeCheck};

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

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        ctx.debug("METHOD CALL ENTER", &self.print());

        // FIXME: No clone here
        let mut call = self.method.clone();

        call.add_arg_front(self.var.clone());

        ctx.debug("DESUGARING TO", &call.print());

        ctx.debug("METHOD CALL EXIT", &self.print());

        call.execute(ctx)
    }

    fn as_bool(&self, ctx: &mut Context) -> Option<bool> {
        let mut call = self.method.clone();
        call.add_arg_front(self.var.clone());

        call.as_bool(ctx)
    }
}

impl TypeCheck for MethodCall {
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType {
        let mut call = self.method.clone();
        call.add_arg_front(self.var.clone());

        call.resolve_type(ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::constructs;
    use crate::*;

    #[test]
    fn t_print() {
        let var = Box::new(JkInt::from(15));
        let method = FunctionCall::new("some_int_func".to_owned(), vec![]);
        let mc = MethodCall::new(var, method);

        assert_eq!(mc.print(), "15.some_int_func()".to_owned())
    }

    #[test]
    fn t_execute() {
        let mut ctx = Context::new();
        let func_dec = constructs::expr("func __first(a: int, b: int) -> int { a }")
            .unwrap()
            .1;
        func_dec.execute(&mut ctx);

        let var1 = Box::new(JkInt::from(1));
        let var2 = Box::new(JkInt::from(2));
        let method = FunctionCall::new("__first".to_owned(), vec![var2]);

        let mc = MethodCall::new(var1, method);

        assert_eq!(mc.execute(&mut ctx).unwrap(), JkInt::from(1).to_instance());
    }

    #[test]
    fn tc_valid_call() {
        jinko! {
            func id(x: int) -> int { x }
            15.id();
            type IntWrapper(inner: int);
            i = IntWrapper(inner: 14);
            // i.inner.id() FIXME: Fix field access and raise issue
        };
    }

    #[test]
    #[ignore] // FIXME: #340
    fn tc_valid_call_multi_arg() {
        jinko! {
            func to_int(self: bool, truthy_value: int) -> int {
                if self {
                    truthy_value
                } else {
                    0
                }
            }
            true.to_int(15);
            false.to_int(188);
        };
    }

    #[test]
    #[ignore] // FIXME: #340
    fn tc_invalid_call_type() {
        jinko! {
            func id(x: int) -> int { x }
            true.id();
        };
    }
}
