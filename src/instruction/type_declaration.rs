use super::{DecArg, InstrKind, Instruction};

use crate::error::JkError;
use crate::interpreter::Interpreter;

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

    /// Get a reference to the name of the type
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get a reference to the type's fields
    pub fn fields(&self) -> &Vec<DecArg> {
        &self.fields
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
