use super::{DecArg, InstrKind, Instruction};

use crate::error::JkError;
use crate::interpreter::Interpreter;

#[derive(Clone, Debug)]
pub struct TypeDec {
    name: String,
    fields: Vec<DecArg>,
}

impl TypeDec {
    pub fn new(name: String, fields: Vec<DecArg>) -> TypeDec {
        TypeDec { name, fields }
    }

    pub fn name(&self) -> &str {
        &self.name
    }

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
