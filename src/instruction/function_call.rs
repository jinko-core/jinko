//! FunctionCalls are used when calling a function. The argument list is given to the
//! function on execution.

use crate::instruction::{FunctionDec, FunctionKind, Var};
use crate::typechecker::TypeCtx;
use crate::{
    typechecker::CheckedType, Context, ErrKind, Error, InstrKind, Instruction, ObjectInstance,
    TypeCheck,
};
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

    /// Get the corresponding declaration from a context
    fn get_declaration(&self, ctx: &mut Context) -> Result<Rc<FunctionDec>, Error> {
        match ctx.get_function(self.name()) {
            // get_function() return a Rc, so this clones the Rc, not the FunctionDec
            Some(f) => Ok(f.clone()),
            // FIXME: Fix Location and input
            None => Err(Error::new(ErrKind::Context)
                .with_msg(format!("cannot find function {}", self.name()))),
        }
    }

    /// Map each argument to its corresponding instruction
    fn map_args(&self, function: &FunctionDec, ctx: &mut Context) {
        for (call_arg, func_arg) in self.args.iter().zip(function.args()) {
            ctx.debug(
                "VAR MAP",
                format!("Mapping `{}` to `{}`", func_arg.name(), call_arg.print()).as_ref(),
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
        builtin_name: &str,
    ) -> Option<ObjectInstance> {
        if ctx.is_builtin(builtin_name) {
            match ctx.call_builtin(builtin_name, self.args.clone()) {
                Ok(value) => value,
                Err(e) => {
                    ctx.error(e);
                    None
                }
            }
        } else {
            ctx.error(Error::new(ErrKind::Context).with_msg(format!(
                "error executing external function `{}`",
                builtin_name
            )));
            None
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

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        let function = match self.get_declaration(ctx) {
            Ok(f) => f,
            Err(e) => {
                ctx.error(e);
                return None;
            }
        };

        if function.fn_kind() == FunctionKind::Ext {
            return self.execute_external_function(ctx, function.name());
        }

        ctx.scope_enter();

        ctx.debug("CALL", self.name());

        self.map_args(&function, ctx);

        let ret_val = function.run(ctx);

        ctx.scope_exit();

        ret_val
    }
}

impl TypeCheck for FunctionCall {
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType {
        // FIXME: This function is very large and should be refactored
        let (args_type, return_type) = match ctx.get_function(self.name()) {
            Some(checked_type) => checked_type,
            // FIXME: This does not account for functions declared later in the code
            None => {
                ctx.error(Error::new(ErrKind::TypeChecker).with_msg(format!(
                    "function `{}` was not declared in this scope",
                    self.name()
                )));
                return CheckedType::Unknown;
            }
        };

        let args_type = args_type.clone();
        let return_type = return_type.clone();

        let mut errors = vec![];
        let mut args = vec![];

        if self.args().len() != args_type.len() {
            errors.push(Error::new(ErrKind::TypeChecker).with_msg(format!(
                "wrong number of arguments \
                    for call to function `{}`: expected {}, got {}",
                self.name(),
                args_type.len(),
                self.args().len()
            )));
        }

        for ((expected_name, expected_ty), given_ty) in args_type.iter().zip(
            self.args
                .iter()
                .map(|given_arg| given_arg.clone().resolve_type(ctx)),
        ) {
            if expected_ty != &given_ty {
                errors.push(Error::new(ErrKind::TypeChecker).with_msg(format!(
                    "invalid type used for function argument: expected `{}`, got `{}`",
                    expected_ty, given_ty
                )));
            }

            args.push((expected_name.clone(), expected_ty.clone()));
        }

        errors.into_iter().for_each(|err| ctx.error(err));
        self.type_args(args, ctx);

        return_type
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::TypeId;
    use crate::{jinko, jinko_fail};

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

        let mut ctx = Context::new();

        // Create a new function with two integers arguments
        let mut f = FunctionDec::new("func0".to_owned(), None);
        f.set_kind(FunctionKind::Func);

        f.set_args(vec![
            DecArg::new("a".to_owned(), TypeId::from("int")),
            DecArg::new("b".to_owned(), TypeId::from("int")),
        ]);

        ctx.add_function(f).unwrap();

        let mut f_call = FunctionCall::new("func0".to_string());

        assert!(f_call.execute(&mut ctx).is_none());
        assert!(
            ctx.error_handler.has_errors(),
            "Given 0 arguments to 2 arguments function"
        );
        ctx.clear_errors();

        f_call.add_arg(Box::new(JkInt::from(12)));

        assert!(f_call.execute(&mut ctx).is_none());
        assert!(
            ctx.error_handler.has_errors(),
            "Given 1 arguments to 2 arguments function"
        );
    }

    #[test]
    fn t_func_call_arg_return() {
        use crate::parser::Construct;
        use crate::value::JkInt;
        use crate::ToObjectInstance;

        let mut i = Context::new();
        let func_dec = Construct::instruction("func __second(f: int, s: int) -> int { s }")
            .unwrap()
            .1;
        let func_call = Construct::instruction("__second(1, 2)").unwrap().1;

        func_dec.execute(&mut i);

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            JkInt::from(2).to_instance()
        );
    }

    #[test]
    fn t_func_call_arg_return_binop() {
        use crate::parser::Construct;
        use crate::value::JkInt;
        use crate::ToObjectInstance;

        let mut i = Context::new();
        let func_dec = Construct::instruction("func add(a: int, b: int) -> int { a + b }")
            .unwrap()
            .1;
        let func_call = Construct::instruction("add(1, 2)").unwrap().1;

        func_dec.execute(&mut i);

        assert_eq!(
            func_call.execute(&mut i).unwrap(),
            JkInt::from(3).to_instance()
        );
    }

    #[test]
    fn t_func_call_variable_return() {
        use crate::parser::Construct;
        use crate::value::JkInt;
        use crate::ToObjectInstance;

        let mut i = Context::new();
        let func_dec = Construct::instruction("func one() -> int { one = 1; one }")
            .unwrap()
            .1;
        let func_call = Construct::instruction("one()").unwrap().1;

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
            take_char(SameButDiff { inner = 'a' })
        };
    }

    #[test]
    fn tc_valid_type_for_arg_complex() {
        jinko! {
            type ComplexType(inner: char);
            func take_char(a: ComplexType) {}
            take_char(ComplexType { inner = 'a' })
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
