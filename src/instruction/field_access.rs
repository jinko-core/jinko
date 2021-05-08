//! FieldAccesses represent an access onto a type instance's members.
//! FIXME: Add doc

use crate::{InstrKind, Instruction, Interpreter, JkError, JkErrKind, Rename};

#[derive(Clone)]
pub struct FieldAccess {
    instance: Box<dyn Instruction>,
    field_name: String,
}

impl FieldAccess {
    /// Create a new field access from the instance accessed and the field seeked
    pub fn new(instance: Box<dyn Instruction>, field_name: String) -> FieldAccess {
        FieldAccess {
            instance,
            field_name,
        }
    }
}

impl Instruction for FieldAccess {
    fn kind(&self) -> InstrKind {
        // A field access can only ever be an expression, since we cannot store statements
        // in a type
        InstrKind::Expression(None)
    }

    fn print(&self) -> String {
        format!("{}.{}", self.instance.print(), self.field_name)
    }

    fn execute(&self, interpreter: &mut Interpreter) -> Result<InstrKind, JkError> {
        interpreter.debug("FIELD ACCESS ENTER", &self.print());

        let calling_instance = match self.instance.execute(interpreter)? {
            InstrKind::Statement | InstrKind::Expression(None) => {
                return Err(JkError::new(
                    JkErrKind::Interpreter,
                    format!(
                        "Instance `{}` is a statement and cannot be accessed",
                        self.instance.print()
                    ),
                    None,
                    self.print(),
                ))
            }
            InstrKind::Expression(Some(i)) => i,
        };
        let field_instance = calling_instance.get_field(&self.field_name)?;

        interpreter.debug("FIELD ACCESS EXIT", &self.print());

        Ok(InstrKind::Expression(Some(field_instance)))
    }
}

impl Rename for FieldAccess {
    fn prefix(&mut self, _: &str) {
        todo!()
    }
}
