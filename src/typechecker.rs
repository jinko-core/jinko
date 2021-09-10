//! The [`TypeCheck`] trait enables a value to get resolved to a type. If the value will
//! need to get its type checked multiple times, then it can implement the [`CachedTypeCheck`]
//! trait on top of it.

use crate::{instruction::TypeId, Context};
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

        write!(f, "{}", ty_str)
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
    fn resolve_type(&self, ctx: &mut Context) -> CheckedType;
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
    fn type_check(&mut self, ctx: &mut Context) {
        if let CheckedType::Unknown = self.get_type() {
            self.set_type(self.resolve_type(ctx))
        }
    }
}
