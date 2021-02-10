use super::{DecArg, InstrKind, Instruction};

use crate::{Interpreter, JkError, Rename};

#[derive(Clone, Debug)]
pub struct TypeDec {
    name: String,
    fields: Vec<DecArg>,
}

impl TypeDec {
    /// Create a new type
    pub fn new(name: String, fields: Vec<DecArg>) -> TypeDec {
        TypeDec { name, fields }
    }

    /// Create a new type from a ype string, with an empty fields vector
    pub fn from(name: &str) -> TypeDec {
        TypeDec {
            name: name.to_string(),
            fields: vec![],
        }
    }

    /// Get a reference to the name of the type
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get a reference to the type's fields
    pub fn fields(&self) -> &Vec<DecArg> {
        &self.fields
    }

    /// Check if a type is a primitive jinko type or not
    pub fn is_primitive_type(&self) -> bool {
        self.name == "bool"
            || self.name == "int"
            || self.name == "float"
            || self.name == "char"
            || self.name == "string"
    }
}

impl Instruction for TypeDec {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug_step(&format!("CUSTOM TYPE {} ENTER", self.name));

        interpreter.add_type(self.clone())?;

        interpreter.debug_step(&format!("CUSTOM TYPE {} EXIT", self.name));

        // Declaring a type is always a statement (for now)
        Ok(InstrKind::Statement)
    }

    fn print(&self) -> String {
        format!("type {} ( {:?} )", self.name, self.fields)
    }
}

impl Rename for TypeDec {
    fn prefix(&mut self, prefix: &str) {
        self.name = format!("{}{}", prefix, self.name);
        self.fields
            .iter_mut()
            .for_each(|field| field.prefix(prefix));
    }
}
