//! A method like call is syntactic sugar over regular function calls. During executions,
//! they get desugared into a normal function call.

use crate::generics::{self, Generic};
use crate::instruction::FunctionCall;
use crate::typechecker::{CheckedType, TypeCtx};
use crate::{log, Context, Error, InstrKind, Instruction, ObjectInstance, SpanTuple, TypeCheck};

#[derive(Clone)]
pub struct MethodCall {
    var: Box<dyn Instruction>,
    method: FunctionCall,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl MethodCall {
    /// Create a new MethodCall from a variable and an associated function
    pub fn new(var: Box<dyn Instruction>, method: FunctionCall) -> MethodCall {
        MethodCall {
            var,
            method,
            cached_type: None,
            location: None,
        }
    }

    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
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
        log!("method call enter: {}", &self.print());

        log!("method call exit: {}", &self.print());

        self.method.execute(ctx)
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for MethodCall {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        log!("typechecking method {}", self.method.name());
        self.method.add_arg_front(self.var.clone());
        if let Some(loc) = &self.location {
            self.method.set_location(loc.clone())
        };

        self.method.type_of(ctx)
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl Generic for MethodCall {
    fn expand(&self, ctx: &mut Context) -> Result<(), Error> {
        let mut call = self.method.clone();
        call.add_arg_front(self.var.clone());
        if let Some(loc) = &self.location {
            call.set_location(loc.clone())
        };

        log!("generic expanding method call: {}", self.method.name());

        call.expand(ctx)
    }

    fn resolve_self(&mut self, ctx: &mut TypeCtx) {
        self.method
            .set_name(generics::mangle(self.method.name(), self.method.generics()));

        let mut call = self.method.clone();
        call.add_arg_front(self.var.clone());
        if let Some(loc) = &self.location {
            call.set_location(loc.clone())
        };

        log!("generic resolving method call: {}", self.method.name());

        call.resolve_self(ctx);
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
        let method = FunctionCall::new("some_int_func".to_owned(), vec![], vec![]);
        let mc = MethodCall::new(var, method);

        assert_eq!(mc.print(), "15.some_int_func()".to_owned())
    }

    #[test]
    fn t_execute() {
        let mut ctx = Context::new();
        let func_dec = constructs::expr(span!("func __first(a: int, b: int) -> int { a }"))
            .unwrap()
            .1;
        func_dec.execute(&mut ctx);

        let var1 = Box::new(JkInt::from(1));
        let var2 = Box::new(JkInt::from(2));
        let method = FunctionCall::new("__first".to_owned(), vec![], vec![var2]);

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
