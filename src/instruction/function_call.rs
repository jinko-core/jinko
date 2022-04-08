//! FunctionCalls are used when calling a function. The argument list is given to the
//! function on execution.
//!
//! A FunctionCall is an aggregation site regarding its arguments.

use std::rc::Rc;

use crate::context::Context;
use crate::error::{ErrKind, Error};
use crate::generics::{self, GenericExpander, GenericMap, GenericUser};
use crate::instance::ObjectInstance;
use crate::instruction::{FunctionDec, FunctionKind, Var};
use crate::instruction::{InstrKind, Instruction};
use crate::location::SpanTuple;
use crate::log;
use crate::typechecker::{CheckedType, SpecializedNode, TypeCheck, TypeCtx, TypeId};

#[derive(Clone)]
pub struct FunctionCall {
    fn_name: String,
    generics: Vec<TypeId>,
    args: Vec<Box<dyn Instruction>>,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl FunctionCall {
    /// Create a new function call and return it
    pub fn new(
        fn_name: String,
        generics: Vec<TypeId>,
        args: Vec<Box<dyn Instruction>>,
    ) -> FunctionCall {
        FunctionCall {
            fn_name,
            generics,
            args,
            cached_type: None,
            location: None,
        }
    }

    /// Add an argument to the beginning of the function call's argument list. This is
    /// only useful for method call desugaring
    pub fn add_arg_front(&mut self, arg: Box<dyn Instruction>) {
        self.args.insert(0, arg)
    }

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

    /// Set the location of a function call
    pub fn set_location(&mut self, loc: SpanTuple) {
        self.location = Some(loc)
    }

    /// Get the corresponding declaration from a context
    fn get_declaration(&self, ctx: &mut Context) -> Rc<FunctionDec> {
        // This clones the Rc, not the function dec
        ctx.get_function(self.name()).cloned().unwrap()
    }

    /// Map each argument to its corresponding instruction
    fn map_args(&self, function: &FunctionDec, ctx: &mut Context) {
        for (call_arg, func_arg) in self.args.iter().zip(function.args()) {
            log!(
                "var map: {}",
                format!("mapping `{}` to `{}`", func_arg.name(), call_arg.print()),
            );

            // Create a new variable, and execute the content of the function argument
            // passed to the call
            let mut new_var = Var::new(func_arg.name().to_owned());
            let mut instance = call_arg.execute_expression(ctx);

            let ty = ctx.get_type(func_arg.get_type()).unwrap();
            //     // Double dereferencing: Some(t) gives us a &Rc<TypeDec>. We dereference
            //     // it to access the Rc, and dereference it again to access the TypeDec.
            let ty = (**ty).clone();

            instance.set_ty(CheckedType::Resolved(ty.into()));

            new_var.set_instance(instance);

            if let Err(e) = ctx.add_variable(new_var) {
                ctx.error(e);
            }
        }
    }

    fn type_args(&self, args: Vec<(String, CheckedType)>, ctx: &mut TypeCtx) {
        ctx.scope_enter();

        // FIXME: Can we refactor this into a more generic and usable aggregation
        // site?
        args.into_iter().for_each(|(arg_name, arg_ty)| {
            if let Err(e) = ctx.declare_var(arg_name, arg_ty) {
                ctx.error(e)
            }
        });

        ctx.scope_exit();
    }

    fn execute_external_function(
        &self,
        ctx: &mut Context,
        dec: &FunctionDec,
    ) -> Option<ObjectInstance> {
        if ctx.is_builtin(dec.name()) {
            ctx.call_builtin(dec.name(), self.args.clone())
        } else {
            #[cfg(feature = "ffi")]
            match crate::ffi::execute(dec, self, ctx) {
                Ok(value) => value,
                Err(e) => {
                    ctx.error(e);
                    None
                }
            }

            #[cfg(not(feature = "ffi"))]
            {
                ctx.error(
                    Error::new(ErrKind::Context)
                        .with_msg(format!(
                    "jinko is not compiled with FFI support. Cannot call `{}` external function",
                    dec.name()
                ))
                        .with_loc(self.location.clone()),
                );
                None
            }
        }
    }

    pub fn generics(&self) -> &Vec<TypeId> {
        &self.generics
    }

    pub fn set_name(&mut self, fn_name: String) {
        self.fn_name = fn_name
    }

    fn resolve_generic_call(
        &mut self,
        function: FunctionDec,
        ctx: &mut TypeCtx,
    ) -> Result<CheckedType, Error> {
        log!(
            "creating specialized fn. function generics: {}, call generics {}",
            function.generics().len(),
            self.generics().len()
        );
        let type_map = GenericMap::create(function.generics(), self.generics(), ctx)?;
        let specialized_name = generics::mangle(function.name(), self.generics());
        log!("specialized name {}", specialized_name);
        if ctx.get_function(&specialized_name).is_none() {
            // FIXME: Remove this clone once we have proper symbols
            let specialized_fn = function.generate(specialized_name.clone(), &type_map, ctx)?;

            ctx.add_specialized_node(SpecializedNode::Func(Box::new(specialized_fn)))?;
        }

        self.fn_name = specialized_name;
        self.generics = vec![];

        // Recursively resolve the type of self now that we changed the
        // function to call
        self.type_of(ctx)
    }
}

impl Instruction for FunctionCall {
    fn kind(&self) -> InstrKind {
        // FIXME: Add logic
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        let mut base = String::from(&self.fn_name);

        if !self.generics.is_empty() {
            base = format!("{}[{}", base, self.generics[0]);

            self.generics
                .iter()
                .skip(1)
                .for_each(|generic| base.push_str(&format!(", {}", generic)));
            base.push(']');
        }

        base.push('(');
        let mut first_arg = true;
        for arg in &self.args {
            if !first_arg {
                base.push_str(", ");
            }

            base.push_str(&arg.print());

            first_arg = false;
        }
        base.push(')');

        base
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        let function = self.get_declaration(ctx);

        if function.fn_kind() == FunctionKind::Ext {
            return self.execute_external_function(ctx, &function);
        }

        ctx.scope_enter();

        log!("call: {}", self.name());

        self.map_args(&function, ctx);

        let ret_val = function.run(ctx);

        ctx.scope_exit();

        ret_val
    }

    fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }
}

impl TypeCheck for FunctionCall {
    fn type_log(&self) -> String {
        self.fn_name.to_string()
    }

    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> Result<CheckedType, Error> {
        log!("typechecking call to {}", self.fn_name);

        // FIXME: Expand generic here instead of resolving later
        // if !self.generics.is_empty() && !ctx.is_second_pass() {
        //     return CheckedType::Later;
        // }

        // FIXME: This function is very large and should be refactored
        let function = match ctx.get_function(self.name()) {
            Some(f) => f.clone(), // FIXME: Remove this clone...
            // FIXME: This does not account for functions declared later in the code
            None => {
                return Err(Error::new(ErrKind::TypeChecker)
                    .with_msg(format!(
                        "function `{}` was not declared in this scope",
                        self.name()
                    ))
                    .with_loc(self.location.clone()))
            }
        };

        // FIXME: Add check for calling non-generic function usign generics

        let (args_type, return_type) = (function.args(), function.ty());
        let args_type = args_type.clone();

        if !function.generics().is_empty() || !self.generics.is_empty() {
            log!("resolving generic call");
            return self.resolve_generic_call(function, ctx);
        }

        log!("resolving args");
        let mut errors = vec![];
        let mut args = vec![];

        if self.args().len() != args_type.len() {
            errors.push(
                Error::new(ErrKind::TypeChecker)
                    .with_msg(format!(
                        "wrong number of arguments \
                    for call to function `{}`: expected {}, got {}",
                        self.name(),
                        args_type.len(),
                        self.args().len()
                    ))
                    .with_loc(self.location.clone()),
            );
        }

        for (dec_arg, given_arg) in args_type.iter().zip(self.args.iter()) {
            // FIXME: Remove clone
            let given_ty = match given_arg.clone().type_of(ctx) {
                Err(e) => {
                    errors.push(e);
                    continue;
                }
                Ok(ty) => ty,
            };
            let expected_ty = CheckedType::Resolved(dec_arg.get_type().clone());
            if expected_ty != given_ty {
                errors.push(
                    Error::new(ErrKind::TypeChecker)
                        .with_msg(format!(
                            "invalid type used for function argument: expected `{}`, got `{}`",
                            expected_ty, given_ty
                        ))
                        .with_loc(given_arg.location().cloned())
                        .with_hint(
                            Error::hint()
                                .with_msg(String::from("argument declared here"))
                                .with_loc(dec_arg.location().cloned()),
                        ),
                );
            }

            args.push((String::from(dec_arg.name()), expected_ty.clone()));
        }

        errors.into_iter().for_each(|err| ctx.error(err));
        self.type_args(args, ctx);

        Ok(return_type.map_or_else(|| CheckedType::Void, |t| CheckedType::Resolved(t.clone())))
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty);
    }
}

impl GenericUser for FunctionCall {
    fn resolve_usages(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) -> Result<(), Error> {
        // For function calls, we can just change our name to one resolved
        // using the generic map. And obviously just visit all of our arguments

        // FIXME: Can we unwrap here?
        log!(generics, "fn name: {}", self.fn_name);
        let dec = match ctx.get_function(&self.fn_name) {
            Some(f) => f,
            None => {
                return Err(Error::new(ErrKind::Generics)
                    .with_msg(format!(
                        "trying to access undeclared function in new specialized function: `{}`",
                        self.fn_name
                    ))
                    .with_loc(self.location.clone()));
            }
        };
        let new_types = type_map.specialized_types(dec.generics())?;

        let new_name = generics::mangle(&self.fn_name, &new_types);
        // FIXME: Avoid the allocation
        let old_name = String::from(&self.fn_name);
        self.fn_name = new_name;
        self.generics = vec![];

        self.args.iter_mut().for_each(|arg| {
            if let Err(e) = arg.resolve_usages(type_map, ctx) {
                ctx.error(e);
            }
        });

        // FIXME: This is ugly as sin
        if ctx.get_specialized_node(&self.fn_name).is_none() && self.fn_name != old_name {
            let demangled = generics::demangle(&self.fn_name);

            // FIXME: Can we unwrap here? Probably not
            let generic_dec = ctx.get_function(demangled).unwrap().clone();
            let specialized_fn = generic_dec.generate(self.fn_name.clone(), type_map, ctx)?;

            ctx.add_specialized_node(SpecializedNode::Func(Box::new(specialized_fn)))
        } else {
            // FIXME: Is that the expected behavior?
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::constructs;
    use crate::{jinko, jinko_fail, span};

    #[test]
    fn t_pretty_print_empty() {
        let function = FunctionCall::new("something".to_owned(), vec![], vec![]);

        assert_eq!(function.print(), "something()");
    }

    // Don't ignore once variable execution is implemented

    #[test]
    fn t_invalid_args_number() {
        use crate::value::JkInt;

        // Create a new function with two integers arguments
        let mut ctx = jinko! {
            func func0(a: int, b: int) {}
        };

        let mut f_call = FunctionCall::new("func0".to_string(), vec![], vec![]);

        assert!(ctx.type_check(&mut f_call).is_err());
        assert!(
            ctx.error_handler.has_errors(),
            "Given 0 arguments to 2 arguments function"
        );
        ctx.clear_errors();

        let mut f_call =
            FunctionCall::new("func0".to_string(), vec![], vec![Box::new(JkInt::from(12))]);

        assert!(ctx.type_check(&mut f_call).is_err());
        assert!(
            ctx.error_handler.has_errors(),
            "Given 1 arguments to 2 arguments function"
        );
    }

    #[test]
    fn t_func_call_arg_return() {
        use crate::instance::ToObjectInstance;
        use crate::value::JkInt;

        let mut i = Context::new();
        let func_dec = constructs::expr(span!("func __second(f: int, s: int) -> int { s }"))
            .unwrap()
            .1;
        let func_call = constructs::expr(span!("__second(1, 2)")).unwrap().1;

        func_dec.execute(&mut i);

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            JkInt::from(2).to_instance()
        );
    }

    #[test]
    fn t_func_call_arg_return_binop() {
        use crate::instance::ToObjectInstance;
        use crate::value::JkInt;

        let mut i = Context::new();
        let func_dec = constructs::expr(span!("func add(a: int, b: int) -> int { a + b }"))
            .unwrap()
            .1;
        let func_call = constructs::expr(span!("add(1, 2)")).unwrap().1;

        func_dec.execute(&mut i);

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            JkInt::from(3).to_instance()
        );
    }

    #[test]
    fn t_func_call_variable_return() {
        use crate::instance::ToObjectInstance;
        use crate::value::JkInt;

        let mut i = Context::new();
        let func_dec = constructs::expr(span!("func one() -> int { one = 1; one }"))
            .unwrap()
            .1;
        let func_call = constructs::expr(span!("one()")).unwrap().1;

        func_dec.execute(&mut i);

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            JkInt::from(1).to_instance()
        );
    }

    #[test]
    fn tc_invalid_type_for_arg() {
        jinko_fail! {
            func take_char(a: char) {}
            take_char("hey");
            take_char(15);
            take_char(true);
            take_char(4.5);
        };
    }

    #[test]
    fn tc_invalid_type_for_arg_complex() {
        jinko_fail! {
            type ComplexType(inner: char);
            type SameButDiff(inner: char);
            func take_char(a: ComplexType) {}
            take_char("hey");
            take_char(15);
            take_char(true);
            take_char(4.5);
            take_char(SameButDiff(inner = 'a'))
        };
    }

    #[test]
    fn tc_valid_type_for_arg_complex() {
        jinko! {
            type ComplexType(inner: char);
            func take_char(a: ComplexType) {}
            take_char(ComplexType(inner: 'a'))
        };
    }

    #[test]
    fn t_call_invalid_builtin() {
        jinko_fail! {
            ext func not_a_builtin();

            not_a_builtin();
        };
    }
}
