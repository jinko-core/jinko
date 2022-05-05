//! FunctionCalls are used when calling a function. The argument list is given to the
//! function on execution.

use std::rc::Rc;

use crate::context::Context;
use crate::error::{ErrKind, Error};
use crate::generics::{self, GenericExpander, GenericList, GenericMap, GenericUser};
use crate::instance::ObjectInstance;
use crate::instruction::{FunctionDec, FunctionKind, Var};
use crate::instruction::{InstrKind, Instruction};
use crate::location::SpanTuple;
use crate::typechecker::{CheckedType, SpecializedNode, TypeCheck, TypeCtx};

#[derive(Clone)]
pub struct FunctionCall {
    fn_name: String,
    generics: GenericList,
    args: Vec<Box<dyn Instruction>>,
    cached_type: Option<CheckedType>,
    location: Option<SpanTuple>,
}

impl FunctionCall {
    /// Create a new function call and return it
    pub fn new(
        fn_name: String,
        generics: GenericList,
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
    fn get_declaration(&self, ctx: &mut Context) -> Result<Rc<FunctionDec>, Error> {
        match ctx.get_function(self.name()) {
            // get_function() return a Rc, so this clones the Rc, not the FunctionDec
            Some(f) => Ok(f.clone()),
            // FIXME: Fix Location and input
            None => Err(Error::new(ErrKind::Context)
                .with_msg(format!("cannot find function {}", self.name()))
                .with_loc(self.location.clone())),
        }
    }

    /// Map each argument to its corresponding instruction
    fn map_args(&self, function: &FunctionDec, ctx: &mut Context) {
        for (call_arg, func_arg) in self.args.iter().zip(function.args()) {
            // Create a new variable, and execute the content of the function argument
            // passed to the call
            let mut new_var = Var::new(func_arg.name().to_owned());
            let mut instance = match call_arg.execute_expression(ctx) {
                Some(i) => i,
                None => {
                    ctx.error(
                        Error::new(ErrKind::Context)
                            .with_msg(format!(
                                "trying to map statement to function argument: {} -> {}",
                                call_arg.print(),
                                func_arg
                            ))
                            .with_loc(func_arg.location().cloned()),
                    );
                    return;
                }
            };

            let ty = match ctx.get_type(func_arg.get_type()) {
                // Double dereferencing: Some(t) gives us a &Rc<TypeDec>. We dereference
                // it to access the Rc, and dereference it again to access the TypeDec.
                Some(t) => (**t).clone(),
                None => {
                    ctx.error(
                        Error::new(ErrKind::Context)
                            .with_msg(format!("type not found: {}", func_arg.get_type().id()))
                            .with_loc(func_arg.location().cloned()),
                    );
                    return;
                }
            };

            instance.set_ty(CheckedType::Resolved(ty.into()));

            new_var.set_instance(instance);

            if let Err(e) = ctx.add_variable(new_var) {
                ctx.error(e);
            }
        }
    }

    fn type_args(&self, args: Vec<(String, CheckedType)>, ctx: &mut TypeCtx) {
        ctx.scope_enter();

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
            match ctx.call_builtin(dec.name(), self.args.clone()) {
                Ok(value) => value,
                Err(e) => {
                    ctx.error(e);
                    None
                }
            }
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

    pub fn generics(&self) -> &GenericList {
        &self.generics
    }

    pub fn set_name(&mut self, fn_name: String) {
        self.fn_name = fn_name
    }

    fn resolve_specialized_call(
        &mut self,
        function: FunctionDec,
        ctx: &mut TypeCtx,
    ) -> CheckedType {
        // let mut generics: Vec<TypeId> = self.generics().iter().map(|g| g.flatten()).collect();
        let type_map = match GenericMap::create(function.generics(), &self.generics, ctx) {
            Ok(map) => map,
            Err(e) => {
                ctx.error(e.with_loc(self.location.clone()));
                return CheckedType::Error;
            }
        };

        // Create missing type declarations from specialized types
        self.generics
            .data()
            .iter()
            .for_each(|g| g.generate_typedec(ctx));

        let specialized_name = generics::mangle(function.name(), &self.generics);
        if ctx.get_function(&specialized_name).is_none() {
            // FIXME: Remove this clone once we have proper symbols
            let specialized_fn = function.generate(specialized_name.clone(), &type_map, ctx);

            ctx.add_specialized_node(SpecializedNode::Func(Box::new(specialized_fn)));
        }

        self.fn_name = specialized_name;
        self.generics = GenericList::empty();

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

        let generics = self.generics.data();
        if !generics.is_empty() {
            base = format!("{}[{}", base, generics[0]);

            generics
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
        let function = match self.get_declaration(ctx) {
            Ok(f) => f,
            Err(e) => {
                ctx.error(e);
                return None;
            }
        };

        if function.fn_kind() == FunctionKind::Ext {
            return self.execute_external_function(ctx, &function);
        }

        ctx.scope_enter();

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
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        // FIXME: Expand generic here instead of resolving later
        // if !self.generics.is_empty() && !ctx.is_second_pass() {
        //     return CheckedType::Later;
        // }

        // FIXME: This function is very large and should be refactored
        let function = match ctx.get_function(self.name()) {
            Some(f) => f.clone(), // FIXME: Remove this clone...
            // FIXME: This does not account for functions declared later in the code
            None => {
                ctx.error(
                    Error::new(ErrKind::TypeChecker)
                        .with_msg(format!(
                            "function `{}` was not declared in this scope",
                            self.name()
                        ))
                        .with_loc(self.location.clone()),
                );
                return CheckedType::Error;
            }
        };

        // FIXME: Add check for calling non-generic function usign generics

        let (args_type, return_type) = (function.args(), function.ty());
        let args_type = args_type.clone();

        if !function.generics().is_empty() || !self.generics.is_empty() {
            return self.resolve_specialized_call(function, ctx);
        }

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
            let given_ty = given_arg.clone().type_of(ctx);
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

        return_type.map_or_else(|| CheckedType::Void, |t| CheckedType::Resolved(t.clone()))
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        self.cached_type.as_ref()
    }

    fn set_cached_type(&mut self, ty: CheckedType) {
        self.cached_type = Some(ty);
    }
}

impl GenericUser for FunctionCall {
    fn resolve_usages(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) {
        // For function calls, we can just change our name to one resolved
        // using the generic map. And obviously just visit all of our arguments

        // FIXME: Can we unwrap here?
        let dec = match ctx.get_function(&self.fn_name) {
            Some(f) => f,
            None => {
                ctx.error(Error::new(ErrKind::Generics)
                    .with_msg(format!("trying to access undeclared function in new specialized function: `{}`", self.fn_name))
                    .with_loc(self.location.clone()));
                return;
            }
        };
        let new_types = match type_map.specialized_types(dec.generics()) {
            Err(e) => {
                ctx.error(e.with_loc(self.location().cloned()));
                return;
            }
            Ok(new_t) => new_t,
        };

        let new_name = generics::mangle(&self.fn_name, &new_types);
        // FIXME: Avoid the allocation
        let old_name = String::from(&self.fn_name);
        self.fn_name = new_name;
        self.generics = GenericList::empty();

        self.args
            .iter_mut()
            .for_each(|arg| arg.resolve_usages(type_map, ctx));

        // FIXME: This is ugly as sin
        if ctx.get_specialized_function(&self.fn_name).is_none() && self.fn_name != old_name {
            let demangled = generics::demangle(&self.fn_name);

            // FIXME: Can we unwrap here? Probably not
            let generic_dec = ctx.get_function(demangled).unwrap().clone();
            let specialized_fn = generic_dec.generate(self.fn_name.clone(), type_map, ctx);
            ctx.add_specialized_node(SpecializedNode::Func(Box::new(specialized_fn)));
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
        let function = FunctionCall::new("something".to_owned(), GenericList::empty(), vec![]);

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

        let mut f_call = FunctionCall::new("func0".to_string(), GenericList::empty(), vec![]);

        assert!(ctx.type_check(&mut f_call).is_err());
        assert!(
            ctx.error_handler.has_errors(),
            "Given 0 arguments to 2 arguments function"
        );
        ctx.clear_errors();

        let mut f_call = FunctionCall::new(
            "func0".to_string(),
            GenericList::empty(),
            vec![Box::new(JkInt::from(12))],
        );

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
