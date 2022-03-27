use std::fmt::{Display, Formatter, Result};

use crate::generics::{Generic, GenericMap};
use crate::location::SpanTuple;
use crate::typechecker::{TypeCtx, TypeId};

#[derive(Clone, Debug, PartialEq)]
pub struct DecArg {
    name: String,
    ty: TypeId,
    location: Option<SpanTuple>,
}

impl DecArg {
    /// Create a new function declaration argument with a name and a type
    pub fn new(name: String, ty: TypeId) -> DecArg {
        DecArg {
            name,
            ty,
            location: None,
        }
    }

    /// Return a reference to the argument's name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a representation of the argument's type
    pub fn get_type(&self) -> &TypeId {
        &self.ty
    }

    /// Get a reference to the argument's location
    pub fn location(&self) -> Option<&SpanTuple> {
        self.location.as_ref()
    }

    /// Set the type of the argument
    pub fn set_type(&mut self, ty: TypeId) {
        self.ty = ty
    }

    /// Set the location of the argument
    pub fn set_location(&mut self, location: SpanTuple) {
        self.location = Some(location)
    }
}

impl Generic for DecArg {
    fn resolve_self(&mut self, type_map: &GenericMap, ctx: &mut TypeCtx) {
        self.ty.resolve_self(type_map, ctx)
    }
}

impl Display for DecArg {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.name, self.ty.id())
    }
}
