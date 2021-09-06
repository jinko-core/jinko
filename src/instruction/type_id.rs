//! A TypeId refers to a type's identifier. For example, the TypeId of `int` is "int".
//! The TypeId of `type Custom(a: int, b: OtherCustom)` is `Custom`.

pub const PRIMITIVE_TYPES: [&str; 5] = ["bool", "int", "float", "char", "string"];

#[derive(Clone, Debug, PartialEq)]
pub struct TypeId {
    id: String,
}

impl TypeId {
    pub fn new(id: String) -> TypeId {
        TypeId { id }
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
