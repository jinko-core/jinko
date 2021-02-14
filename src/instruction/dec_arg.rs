use crate::instruction::TypeId;
use crate::Rename;

#[derive(Clone, Debug, PartialEq)]
pub struct DecArg {
    name: String,
    ty: TypeId,
}

impl DecArg {
    /// Create a new function declaration argument with a name and a type
    pub fn new(name: String, ty: TypeId) -> DecArg {
        DecArg { name, ty }
    }

    /// Return a reference to the argument's name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a representation of the argument's type
    pub fn get_type(&self) -> &TypeId {
        &self.ty
    }
}

impl Rename for DecArg {
    fn prefix(&mut self, prefix: &str) {
        self.name = format!("{}{}", prefix, self.name);
        self.ty.prefix(prefix);
    }
}
