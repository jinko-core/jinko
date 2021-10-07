//! The [`TypeCheck`] trait enables a value to get resolved to a type. If the value will
//! need to get its type checked multiple times, then it can implement the [`CachedTypeCheck`]
//! trait on top of it.

use crate::{instruction::TypeId, Context, Error, ScopeMap};
use colored::Colorize;
use std::fmt::{Display, Formatter, Result};

/// The [`CheckedType`] enum contains three possible states about the type. Either the
/// type has been properly resolved to something, or it corresponds to a Void type. If the
/// type has not been resolved yet, it can be unknown.
#[derive(Clone, PartialEq, Debug)]
pub enum CheckedType {
    Resolved(TypeId),
    Void,
    Unknown,
}

impl Default for CheckedType {
    fn default() -> CheckedType {
        CheckedType::Unknown
    }
}

impl Display for CheckedType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        let ty_str = match self {
            CheckedType::Resolved(ty) => ty.id(),
            CheckedType::Void => "void",
            CheckedType::Unknown => "!!unknown!!",
        };

        write!(f, "{}", ty_str.purple())
    }
}

struct FunctionType {
    /// We need to keep track of the function's arguments name and expected type
    args_ty: Vec<(String, CheckedType)>,
    return_ty: CheckedType,
}

struct CustomTypeType {
    self_ty: CheckedType,
    fields_ty: Vec<(String, CheckedType)>,
}

// TODO: Should we factor this into a `Context` trait? All contexts will share some
// similarities, such as the ability to emit errors or enter and exit scopes
/// The [`TypeCtx`]'s role is to keep track of declared types based on the scope they
/// were created in. In doing so, it allows the [`Instruction`]s to perform a type lookup
/// in order to resolve to a concrete type.Each declaration (First [`VarAssign`],
/// [`FunctionDec`]s and [`TypeDec`]s) can also declare a new type and make it available
/// to all instructions in the avaialble scopes.
pub struct TypeCtx<'ctx> {
    /// Reference to the original context in order to emit errors properly
    pub(crate) context: &'ctx mut Context,
    /// The Type Context stores [`CheckedType`]s for all three generics kept in the scope
    /// map: Variables, Functions and Types.
    /// For functions, we keep a vector of argument types as well as the return type.
    /// Custom types need to keep a type for themselves, as well as types for all their fields
    types: ScopeMap<CheckedType, FunctionType, CustomTypeType>,
}

impl<'ctx> TypeCtx<'ctx> {
    /// Create a new empty [`TypeCtx`]
    pub fn new(ctx: &'ctx mut Context) -> TypeCtx {
        // FIXME: This doesn't contain builtins
        let mut ctx = TypeCtx {
            context: ctx,
            types: ScopeMap::new(),
        };

        ctx.scope_enter();

        ctx
    }

    /// Enter a new scope. This is the same as lexical scopes
    pub fn scope_enter(&mut self) {
        self.types.scope_enter()
    }

    /// Exit a previously created scope. This is the same as lexical scopes
    pub fn scope_exit(&mut self) {
        self.types.scope_exit()
    }

    /// Declare a newly-created variable's type
    pub fn declare_var(&mut self, name: String, ty: CheckedType) {
        // We can unwrap since this is an interpreter error if we can't add a new
        // type to the scope map
        self.types.add_variable(name, ty).unwrap();
    }

    /// Declare a newly-created function's type
    pub fn declare_function(
        &mut self,
        name: String,
        args_ty: Vec<(String, CheckedType)>,
        return_ty: CheckedType,
    ) {
        // We can unwrap since this is an interpreter error if we can't add a new
        // type to the scope map
        self.types
            .add_function(name, FunctionType { args_ty, return_ty })
            .unwrap();
    }

    /// Declare a newly-created custom type
    pub fn declare_custom_type(
        &mut self,
        name: String,
        self_ty: CheckedType,
        fields_ty: Vec<(String, CheckedType)>,
    ) {
        // We can unwrap since this is an interpreter error if we can't add a new
        // type to the scope map
        self.types
            .add_type(name, CustomTypeType { self_ty, fields_ty })
            .unwrap();
    }

    /// Access a previously declared variable's type
    pub fn get_var(&mut self, name: &str) -> Option<&CheckedType> {
        self.types.get_variable(name)
    }

    /// Access a previously declared function's type
    pub fn get_function(
        &mut self,
        name: &str,
    ) -> Option<(&Vec<(String, CheckedType)>, &CheckedType)> {
        self.types
            .get_function(name)
            .map(|func| (&func.args_ty, &func.return_ty))
    }

    /// Access a previously declared custom type
    pub fn get_custom_type(
        &mut self,
        name: &str,
    ) -> Option<(&CheckedType, &Vec<(String, CheckedType)>)> {
        self.types
            .get_type(name)
            .map(|custom_type| (&custom_type.self_ty, &custom_type.fields_ty))
    }

    /// Create a new error to propagate to the original context
    pub fn error(&mut self, err: Error) {
        self.context.error(err)
    }
}

/// The [`TypeCheck`] trait allows an [`Instruction`] to see its type resolved statically.
/// There are three possible return values:
///     - Resolve(type): This means that the type of the [`Instruction`] was abled to
///     get resolved statically
///     - Void: The [`Instruction`] is not of any type. It corresponds to a statement.
///     - Unknown: This means that even after the typechecking pass, the instruction's
///     type is still unclear.
pub trait TypeCheck {
    /// Go through the context in order to figure out the type of an instruction.
    /// This function should report errors using the context, and the [`ErrKind::TypeCheck`]
    /// error kind.
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType;
}

/// Some [`Instruction`]s need to have their type checked multiple times. For example, a
/// function might be called in multiple places, by various vaiables. These instructions
/// can "cache" their type in order to not go through the resolver each time
pub trait CachedTypeCheck: TypeCheck {
    /// Store the given type somewhere in order to cache it
    fn set_type(&mut self, ty: CheckedType);

    /// Return a reference to the cached type, previously stored using [`set_type()`]
    fn get_type(&self) -> &CheckedType;

    /// If the type is not known yet, compute it by going through the [`TypeCheck`]
    /// resolver. Otherwise, fetch it from the cached instance
    fn type_check(&mut self, ctx: &mut TypeCtx) {
        if let CheckedType::Unknown = self.get_type() {
            self.set_type(self.resolve_type(ctx))
        }
    }
}
