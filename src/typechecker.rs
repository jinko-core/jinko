//! The [`TypeCheck`] trait enables a value to get resolved to a type.
//! FIXME: MOAR DOC

use crate::Context;

pub enum CheckedType<T> {
    Resolved(T),
    Void,
    Unknown,
}

pub trait TypeCheck<T> {
    fn set_type(&mut self, ty: CheckedType<T>);
    fn get_type(&self) -> CheckedType<&T>;
    fn resolve_type(&self, ctx: &mut Context) -> CheckedType<T>;

    fn type_check(&mut self, ctx: &mut Context) {
        if let CheckedType::Unknown = self.get_type() {
            self.set_type(self.resolve_type(ctx))
        }
    }
}
