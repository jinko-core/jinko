//! The [`Generic`] trait is required to enable the generic type expansion of
//! variables, type and function declarations, type instantiations and function
//! calls. Its basic goal is to replace all instances of `T` with an actual [`TypeId`].

use std::collections::HashMap;

use crate::instruction::TypeId;
use crate::typechecker::TypeCtx;

pub type GenericMap = HashMap<String, TypeId>;

pub trait Generic {
    fn expand(&self, type_ctx: &mut TypeCtx);

    fn resolve_self(&mut self, type_map: GenericMap);

    fn generate_new(&self, _type_map: GenericMap) -> Option<Box<Self>> {
        None
    }
}
