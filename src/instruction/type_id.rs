//! A TypeId refers to a type's identifier. For example, the TypeId of `int` is "int".
//! The TypeId of `type Custom(a: int, b: OtherCustom)` is `Custom`.

use crate::Rename;

const PRIMITIVE_IDS: [&str; 5] = ["bool", "int", "float", "char", "string"];

#[derive(Clone, Debug, PartialEq)]
pub struct TypeId(String);

impl TypeId {
    pub fn new(id: String) -> TypeId {
        TypeId { 0: id }
    }

    pub fn id(&self) -> &str {
        &self.0
    }
}

impl Rename for TypeId {
    fn prefix(&mut self, prefix: &str) {
        match PRIMITIVE_IDS.contains(&self.0.as_str()) {
            // No need to rename primitive types
            true => {}
            false => self.0 = format!("{}{}", prefix, self.0),
        }
    }
}
