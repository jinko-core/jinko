//! The [`TypeCheck`] trait enables a value to get resolved to a type.
//! FIXME: MOAR DOC

use crate::{Context, instruction::TypeDec};

/// In order to avoid recomputing various types multiple times, instruction types should
/// keep a [`CheckedType`] instance as a member function, which should be set and fetched
/// using [`set_type()`] and [`get_type`]
#[derive(Clone)]
pub enum CheckedType {
    Resolved(TypeDec),
    Void,
    Unknown,
}

impl Default for CheckedType {
    fn default() -> CheckedType {
        CheckedType::Unknown
    }
}

pub trait TypeCheck {
    fn set_type(&mut self, ty: CheckedType);
    fn get_type(&self) -> &CheckedType;
    fn resolve_type(&self, ctx: &mut Context) -> CheckedType;

    fn type_check(&mut self, ctx: &mut Context) {
        if let CheckedType::Unknown = self.get_type() {
            self.set_type(self.resolve_type(ctx))
        }
    }
}

#[macro_export]
macro_rules! type_cache {
    ($member:ident) => {
        fn set_type(&mut self, ty: CheckedType) {
            self.$member = ty
        }

        fn get_type(&self) -> &CheckedType {
            &self.$member
        }
    }
}
