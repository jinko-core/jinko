//! FunctionCalls are used when calling a function. The argument list is given to the
//! function on execution.

use crate::generics::GenericMap;
use crate::instruction::{FunctionDec, FunctionKind, Var};
use crate::typechecker::{CheckedType, TypeCtx, TypeId};
use crate::{
    generics, log, Context, ErrKind, Error, Generic, InstrKind, Instruction, ObjectInstance,
    SpanTuple, TypeCheck,
};
use std::rc::Rc;

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
            log!(
                "var map: {}",
                format!("mapping `{}` to `{}`", func_arg.name(), call_arg.print()),
            );

            // Create a new variable, and execute the content of the function argument
            // passed to the call
            let mut new_var = Var::new(func_arg.name().to_owned());
            let mut instance = match call_arg.execute_expression(ctx) {
                Some(i) => i,
                None => {
                    ctx.error(Error::new(ErrKind::Context).with_msg(format!(
                        "trying to map statement to function argument: {} -> {}",
                        call_arg.print(),
                        func_arg
                    )));
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
                            .with_msg(format!("type not found: {}", func_arg.get_type().id())),
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

    pub fn generics(&self) -> &Vec<TypeId> {
        &self.generics
    }

    pub fn set_name(&mut self, fn_name: String) {
        self.fn_name = fn_name
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
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        log!("typechecking call to {}", self.fn_name);

        if !self.generics.is_empty() && !ctx.is_second_pass() {
            return CheckedType::Later;
        }

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

        // If the declaration contains generics but not the call, typecheck later still
        if !function.generics().is_empty() && !ctx.is_second_pass() {
            return CheckedType::Later;
        }

        let (args_type, return_type) = (function.args(), function.ty());

        let args_type = args_type.clone();

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

        for (dec_arg, given_ty) in args_type.iter().zip(
            self.args
                .iter()
                .map(|given_arg| given_arg.clone().type_of(ctx)),
        ) {
            let expected_ty = CheckedType::Resolved(dec_arg.get_type().clone());
            if expected_ty != given_ty {
                errors.push(Error::new(ErrKind::TypeChecker).with_msg(format!(
                    "invalid type used for function argument: expected `{}`, got `{}`",
                    expected_ty, given_ty
                )));
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

impl Generic for FunctionCall {
    fn expand(&self, ctx: &mut Context) {
        let generic_name = generics::mangle(&self.fn_name, &self.generics);
        let dec = ctx.typechecker.get_function(&generic_name);

        // FIXME: This is a little weird
        if self.generics.is_empty() && dec.map_or(false, |f| f.generics().is_empty()) {
            return;
        }

        if dec.is_none() {
            // We can return here since this is a typechecking error and should have been
            // caught already in an earlier pass
            let dec = match ctx.typechecker.get_function(&self.fn_name) {
                Some(dec) => dec.clone(),
                None => return,
            };

            if !self.generics.is_empty() && dec.generics().is_empty() {
                // FIXME: Format generic list in error too
                ctx.error(
                    Error::new(ErrKind::Generics)
                        .with_msg(format!(
                            "calling non-generic function with generic arguments: `{}`",
                            self.name()
                        ))
                        .with_loc(self.location.clone()),
                );
                return;
            }

            let type_map =
                match GenericMap::create(dec.generics(), &self.generics, &mut ctx.typechecker) {
                    Err(e) => {
                        ctx.error(e);
                        return;
                    }
                    Ok(m) => m,
                };

            log!("generic_name: {}", &generic_name);

            let mut new_fn = match dec.from_type_map(generic_name, &type_map, ctx) {
                Ok(f) => f,
                Err(e) => {
                    ctx.error(e);
                    return;
                }
            };

            if let Err(e) = ctx.type_check(&mut new_fn) {
                // FIXME: This should probably be a generic error instead
                // FIXME: The name is also mangled and shouldn't be
                ctx.error(e);
            } else {
                ctx.add_function(new_fn).unwrap();
            }
        }
    }

    fn resolve_self(&mut self, ctx: &mut TypeCtx) {
        let generic_name = generics::mangle(&self.fn_name, &self.generics);
        let dec = ctx.get_function(&generic_name);

        // FIXME: This is a little weird
        if self.generics.is_empty() && dec.map_or(false, |f| f.generics().is_empty()) {
            return;
        }

        // FIXME: This doesnt have all the actual types? T (b's type) is missing (464_arg_ty.jk)
        // So we actually need to only map argument from the dec's arguments which are generic. Not
        // all of them. Get a list of indexes or something from the dec and fetch those argument
        // types only
        // FIXME: We can only have actual types here: Not void, not unknown, nothing
        let _resolved_types: Vec<TypeId> = self
            .args
            .iter_mut()
            .map(|arg| match arg.type_of(ctx) {
                CheckedType::Resolved(ty) => ty,
                _ => TypeId::void(),
                // FIXME: Is this the correct behavior? The error
                // will already have been emitted at this point
            })
            .collect();

        self.fn_name = generic_name;
        self.generics = vec![];
        self.cached_type = None;
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
        use crate::value::JkInt;
        use crate::ToObjectInstance;

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
        use crate::value::JkInt;
        use crate::ToObjectInstance;

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
        use crate::value::JkInt;
        use crate::ToObjectInstance;

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
