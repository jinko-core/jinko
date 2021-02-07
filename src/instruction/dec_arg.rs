use crate::Rename;

// FIXME: Shouldn't be a String
pub type Ty = String;

#[derive(Clone, Debug)]
pub struct DecArg {
    name: String,
    ty: Ty,
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

    /// Return a reference to the argument's type
    pub fn ty(&self) -> &Ty {
        &self.ty
    }
}

impl Rename for DecArg {
    fn prefix(&mut self, prefix: &str) {
        self.name = format!("{}{}", prefix, self.name);
        self.ty = format!("{}{}", prefix, self.ty);
    }
}
