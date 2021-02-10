use crate::instruction::TypeDec;
use crate::Rename;

#[derive(Clone, Debug, PartialEq)]
pub struct DecArg {
    name: String,
    ty: String,
}

impl DecArg {
    /// Create a new function declaration argument with a name and a type
    pub fn new(name: String, ty: String) -> DecArg {
        DecArg { name, ty }
    }

    /// Return a reference to the argument's name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Return a representation of the argument's type
    pub fn get_type(&self) -> TypeDec {
        TypeDec::from(self.ty.as_str())
    }
}

impl Rename for DecArg {
    fn prefix(&mut self, prefix: &str) {
        self.name = format!("{}{}", prefix, self.name);

        // If the type is a primitive one, no need to rename it
        if !TypeDec::from(self.get_type()).is_primitive_type() {
            self.ty = format!("{}{}", prefix, self.ty);
        }
    }
}
