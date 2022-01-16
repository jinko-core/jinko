//! Function Declarations are used when adding a new function to the source. They contain
//! a name, a list of required arguments as well as an associated code block

use crate::generics::GenericMap;
use crate::instruction::{Block, DecArg, InstrKind, Instruction, TypeId};
use crate::typechecker::{CheckedType, TypeCtx};
use crate::Generic;
use crate::{log, Context, ErrKind, Error, ObjectInstance, TypeCheck};

/// What "kind" of function is defined. There are four types of functions in jinko,
/// the normal ones, the external ones, the unit tests and the mocks
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum FunctionKind {
    Unknown,
    Func,
    Ext,
    Test,
    Mock,
}

#[derive(Clone)]
pub struct FunctionDec {
    name: String,
    ty: Option<TypeId>,
    kind: FunctionKind,
    generics: Vec<TypeId>,
    args: Vec<DecArg>,
    block: Option<Block>,
    typechecked: bool,
}

impl FunctionDec {
    /// Create a new function declaration with a given name, no args and no code block
    pub fn new(
        name: String,
        ty: Option<TypeId>,
        generics: Vec<TypeId>,
        args: Vec<DecArg>,
    ) -> FunctionDec {
        FunctionDec {
            name,
            ty,
            kind: FunctionKind::Unknown,
            generics,
            args,
            block: None,
            typechecked: false,
        }
    }

    /// Generate a new instance of [`FunctionDec`] from a given generic type map
    pub fn from_type_map(
        &self,
        mangled_name: String,
        type_map: &GenericMap,
        ctx: &mut Context,
    ) -> Result<FunctionDec, Error> {
        let mut new_fn = self.clone();
        new_fn.name = mangled_name;
        new_fn.generics = vec![];

        let mut is_err = false;

        new_fn
            .args
            .iter_mut()
            .zip(self.args().iter())
            .filter(|(_, generic)| self.generics().contains(generic.get_type()))
            .for_each(|(new_arg, old_generic)| {
                let new_type = match type_map.get_match(old_generic.get_type()) {
                    Ok(t) => t,
                    Err(e) => {
                        ctx.error(e);
                        is_err = true;
                        TypeId::void()
                    }
                };
                new_arg.set_type(new_type);
            });

        if let Some(ret_ty) = &self.ty {
            if self.generics().contains(ret_ty) {
                let new_ret_ty = type_map.get_match(ret_ty)?;
                new_fn.ty = Some(new_ret_ty);
            }
        }

        // FIXME: We also need to generate a new version of each instruction in the
        // block
        // if let Some(b) = &mut new_fn.block {
        //     b.resolve_self(ctx);
        // }

        match is_err {
            true => Err(Error::new(ErrKind::Generics)),
            false => Ok(new_fn),
        }
    }

    pub fn generics(&self) -> &Vec<TypeId> {
        &self.generics
    }

    /// Set the block of a given function declaration. This is useful since parsing a
    /// function's block comes after the function signature.
    pub fn set_block(&mut self, block: Block) {
        self.block = Some(block)
    }

    /// Add an instruction to the function declaration, in order. This is mostly useful
    /// when adding instructions to the entry point of the context, since parsing
    /// directly gives a block to the function
    pub fn add_instruction(&mut self, instruction: Box<dyn Instruction>) -> Result<(), Error> {
        match &mut self.block {
            Some(b) => {
                b.add_instruction(instruction);
                Ok(())
            }
            None => Err(Error::new(ErrKind::Context).with_msg(format!(
                "function {} has no instruction block. It might be an extern function or an error",
                self.name
            ))),
        }
    }

    /// Return a reference to the function's name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a reference to the function's return type
    pub fn ty(&self) -> Option<&TypeId> {
        self.ty.as_ref()
    }

    /// Set the type of the function
    pub fn set_ty(&mut self, ty: Option<TypeId>) {
        self.ty = ty
    }

    /// Return the kind of a function
    pub fn fn_kind(&self) -> FunctionKind {
        self.kind
    }

    /// Set the kind of a function. This cannot be done at initialization due to the
    /// parser's nature
    pub fn set_kind(&mut self, kind: FunctionKind) {
        self.kind = kind
    }

    /// Return a reference to the function's arguments
    pub fn args(&self) -> &Vec<DecArg> {
        &self.args
    }

    /// Set the vector of arguments that the function should handle
    pub fn set_args(&mut self, args: Vec<DecArg>) {
        self.args = args
    }

    pub fn set_generics(&mut self, generics: Vec<TypeId>) {
        self.generics = generics
    }

    /// Return a reference to the function's block
    pub fn block(&self) -> Option<&Block> {
        self.block.as_ref()
    }

    /// Return a mutable reference to the function's block
    pub fn block_mut(&mut self) -> Option<&mut Block> {
        self.block.as_mut()
    }

    /// Run through the function as if it was called. This is useful for setting
    /// an entry point into the interpreter and executing it
    pub fn run(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        self.block().unwrap().execute(ctx)
    }
}

impl Instruction for FunctionDec {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        log!("funcdec enter: {}", self.name());

        match self.fn_kind() {
            FunctionKind::Func | FunctionKind::Ext => {
                if let Err(e) = ctx.add_function(self.clone()) {
                    ctx.error(e);
                }
            }
            FunctionKind::Test => {
                if let Err(e) = ctx.add_test(self.clone()) {
                    ctx.error(e);
                }
            }
            FunctionKind::Mock | FunctionKind::Unknown => ctx.error(
                Error::new(ErrKind::Context)
                    .with_msg(format!("unknown type for function {}", self.name())),
            ),
        }

        log!("funcdec exit: {}", self.name());

        None
    }

    fn print(&self) -> String {
        let mut base = String::from(match self.kind {
            FunctionKind::Func => "func",
            FunctionKind::Ext => "ext func",
            FunctionKind::Test => "test",
            FunctionKind::Mock => "mock",
            FunctionKind::Unknown => "UNKNOWN",
        });

        base = format!("{} {}", base, self.name);

        if !self.generics.is_empty() {
            base.push('[');
            base.push_str(self.generics.first().unwrap().id());
            let generic_str = self
                .generics
                .iter()
                .skip(1)
                .fold(String::new(), |acc, ty_id| {
                    format!("{}, {}", acc, ty_id.id())
                });
            base.push_str(&generic_str);
            base.push(']');
        }

        base.push('(');
        if !self.args.is_empty() {
            base.push_str(&format!("{}", self.args().iter().next().unwrap()));
            let arg_str = self
                .args
                .iter()
                .skip(1)
                .fold(String::new(), |acc, field| format!("{}, {}", acc, field));
            base.push_str(&arg_str);
        }
        base.push(')');

        base = match &self.ty {
            Some(ty) => format!("{} -> {}", base, ty.id()),
            None => base,
        };

        match &self.block {
            Some(block) => format!("{} {}", base, block.print()),
            None => format!("{} {{}}", base),
        }
    }
}

impl TypeCheck for FunctionDec {
    fn resolve_type(&mut self, ctx: &mut TypeCtx) -> CheckedType {
        // If a declaration contains generic types, there is no point in type-checking
        // it: All the methods or field accesses will, by definition, not exist, since
        // the generic types do not exist yet
        if !self.generics.is_empty() {
            // Just declare the function so we have it in the context and can
            // duplicate it
            if let Err(e) = ctx.declare_function(self.name().into(), self.clone()) {
                ctx.error(e);
                return CheckedType::Error;
            }

            return CheckedType::Later;
        }

        // FIXME: Remove clone?
        if let Err(e) = ctx.declare_function(self.name().into(), self.clone()) {
            ctx.error(e);
        }

        ctx.scope_enter();

        // FIXME: Both return_ty and args_ty can be factored from the `ty_declare`
        // function
        let return_ty = match &self.ty {
            // FIXME: Remove clone?
            Some(ty) => CheckedType::Resolved(ty.clone()),
            None => CheckedType::Void,
        };

        let args_ty: Vec<(String, CheckedType)> = self
            .args
            .iter()
            .map(|dec_arg| {
                (
                    dec_arg.name().to_string(),
                    CheckedType::Resolved(dec_arg.get_type().clone()),
                )
            })
            .collect();

        args_ty.iter().for_each(|(name, ty)| {
            if let Err(e) = ctx.declare_var(name.clone(), ty.clone()) {
                ctx.error(e);
            }
        });

        // If the function has no block, trust the declaration
        if let Some(b) = &mut self.block {
            let block_ty = b.type_of(ctx);

            if block_ty != return_ty {
                ctx.error(Error::new(ErrKind::TypeChecker).with_msg(format!(
                    "invalid type returned in function `{}`: expected type {}, found type {}",
                    self.name(),
                    return_ty,
                    block_ty
                )));

                ctx.scope_exit();

                return CheckedType::Error;
            }
        }

        ctx.scope_exit();

        CheckedType::Void
    }

    fn set_cached_type(&mut self, _ty: CheckedType) {
        self.typechecked = true
    }

    fn cached_type(&self) -> Option<&CheckedType> {
        match self.typechecked {
            true => Some(&CheckedType::Void),
            false => None,
        }
    }
}

impl Generic for FunctionDec {}

impl Default for FunctionDec {
    fn default() -> Self {
        FunctionDec::new(String::new(), None, vec![], vec![])
    }
}

impl std::fmt::Debug for FunctionDec {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.print())
    }
}

impl From<&str> for FunctionKind {
    fn from(function_kind: &str) -> FunctionKind {
        match function_kind {
            "func" => FunctionKind::Func,
            "test" => FunctionKind::Test,
            "mock" => FunctionKind::Mock,
            "ext" => FunctionKind::Ext,
            _ => FunctionKind::Unknown,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{instruction::TypeId, jinko, parser::constructs};

    #[test]
    fn simple_no_arg() {
        let mut function = FunctionDec::new("fn".to_owned(), None, vec![], vec![]);
        function.set_kind(FunctionKind::Func);

        assert_eq!(function.print(), "func fn() {}");
    }

    #[test]
    fn simple_args() {
        let args = vec![
            DecArg::new("arg0".to_owned(), TypeId::from("int")),
            DecArg::new("arg1".to_owned(), TypeId::from("int")),
        ];

        let mut function =
            FunctionDec::new("fn".to_owned(), Some(TypeId::from("int")), vec![], args);
        function.set_kind(FunctionKind::Func);

        assert_eq!(function.print(), "func fn(arg0: int, arg1: int) -> int {}");
    }

    #[test]
    fn tc_ext_func() {
        let mut function =
            FunctionDec::new("fn".to_owned(), Some(TypeId::from("int")), vec![], vec![]);
        function.set_kind(FunctionKind::Ext);

        let mut ctx = Context::new();

        assert_eq!(ctx.type_check(&mut function).unwrap(), CheckedType::Void);
        assert!(!ctx.error_handler.has_errors());
    }

    #[test]
    fn tc_valid() {
        let mut function =
            FunctionDec::new("fn".to_owned(), Some(TypeId::from("int")), vec![], vec![]);
        function.set_kind(FunctionKind::Func);

        let block = constructs::block("{ 15 }").unwrap().1;
        function.set_block(block);

        let mut ctx = Context::new();

        assert_eq!(ctx.type_check(&mut function).unwrap(), CheckedType::Void);
        assert!(!ctx.error_handler.has_errors());
    }

    #[test]
    fn tc_invalid() {
        let mut function = FunctionDec::new(
            "fn".to_owned(),
            Some(TypeId::from("string")),
            vec![],
            vec![],
        );
        function.set_kind(FunctionKind::Func);

        let block = constructs::block("{ 15 }").unwrap().1;
        function.set_block(block);

        let mut ctx = Context::new();

        assert!(ctx.type_check(&mut function).is_err());
        assert!(ctx.error_handler.has_errors());
    }

    #[test]
    fn tc_function_dec_same_args() {
        jinko! {
            func takes_int_i(i: int) {}
            func return_int_i(i: int) -> int { i }
        };
    }
}
