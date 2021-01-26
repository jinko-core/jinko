use super::{DecArg, InstrKind, Instruction};

use crate::error::JinkoError;
use crate::interpreter::Interpreter;

#[derive(Clone)]
pub struct CustomType {
    name: String,
    fields: Vec<DecArg>,
}

impl CustomType {
    pub fn new(name: String, fields: Vec<DecArg>) -> CustomType {
        CustomType { name, fields }
    }

    pub fn name(&self) -> &str {
        &self.name
    }
}

impl Instruction for CustomType {
    fn kind(&self) -> InstrKind {
        InstrKind::Statement
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JinkoError> {
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
