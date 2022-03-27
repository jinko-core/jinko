//! A method like call is syntactic sugar over regular function calls. During executions,
//! they get desugared into a normal function call.

use crate::context::Context;
use crate::generics::Generic;
use crate::instance::ObjectInstance;
use crate::instruction::FunctionCall;
use crate::instruction::{InstrKind, Instruction};
use crate::location::SpanTuple;
use crate::log;
use crate::typechecker::{CheckedType, TypeCheck, TypeCtx};

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
        log!("desugared method call enter: `{}`", &self.method.print());

        let mut call = self.method.clone();

        call.add_arg_front(self.var.clone());
        if let Some(loc) = &self.location {
            call.set_location(loc.clone())
        };

        log!("desugared method call exit: `{}`", &self.method.print());

        call.execute(ctx)
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for MethodCall {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        log!("typechecking method `{}`", self.method.name());
        log!("desugaring method `{}`", self.print());

        let mut call = self.method.clone();

        call.add_arg_front(self.var.clone());
        if let Some(loc) = &self.location {
            call.set_location(loc.clone())
        };

        log!("desugared method to `{}`", call.print());

        let res = call.type_of(ctx);

        // FIXME: Figure out a way to do this without the extra clone, which
        // is useless. We should be able to do desugaring here (or before)
        // and simply resolve to the new mangled name if necessary
        self.method.set_name(call.name().to_string());

        res
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty)
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }
}

impl Generic for MethodCall {
    fn resolve_self(&mut self, type_map: &crate::generics::GenericMap, ctx: &mut TypeCtx) {
        // FIXME: Can we avoid adding the argument here?
        self.method.resolve_self(type_map, ctx);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instance::ToObjectInstance;
    use crate::parser::constructs;
    use crate::value::JkInt;
    use crate::{jinko, span};

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
        let mut func_dec = constructs::expr(span!("func __first(a: int, b: int) -> int { a }"))
            .unwrap()
            .1;
        ctx.type_check(&mut *func_dec).unwrap();
        func_dec.execute(&mut ctx);

        let var1 = Box::new(JkInt::from(1));
        let var2 = Box::new(JkInt::from(2));
        let method = FunctionCall::new("__first".to_owned(), vec![], vec![var2]);

        let mut mc = MethodCall::new(var1, method);
        ctx.type_check(&mut mc).unwrap();

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
