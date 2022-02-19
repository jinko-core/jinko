//! A TypeId refers to a type's identifier. For example, the TypeId of `int` is "int".
//! The TypeId of `type Custom(a: int, b: OtherCustom)` is `Custom`.

use std::fmt::{Display, Formatter, Result as FmtResult};
use std::hash::Hash;

use colored::Colorize;

use crate::instruction::TypeDec;

pub const PRIMITIVE_TYPES: [&str; 5] = ["bool", "int", "float", "char", "string"];

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TypeId {
    id: String,
}

impl TypeId {
    pub const fn new(id: String) -> TypeId {
        TypeId { id }
    }

    pub fn void() -> TypeId {
        TypeId::new(String::from("void"))
    }

    pub fn id(&self) -> &str {
        &self.id
    }

    pub fn is_primitive(&self) -> bool {
        PRIMITIVE_TYPES.contains(&self.id.as_str())
    }
}

impl From<&str> for TypeId {
    fn from(s: &str) -> Self {
        TypeId::new(s.to_string())
    }
}

impl From<&TypeDec> for TypeId {
    fn from(td: &TypeDec) -> Self {
        TypeId::from(td.name())
    }
}

impl From<TypeDec> for TypeId {
    fn from(td: TypeDec) -> Self {
        TypeId::from(&td)
    }
}

impl Display for TypeId {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        write!(f, "{}", self.id.purple())
    }
}
