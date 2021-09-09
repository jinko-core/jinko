//! Return construct is used to return early from a function
//! ```
//! return
//! ```
//!
//! It can be used to return values
//!
//! ```
//! return 42
//! ```

use crate::instruction::{InstrKind, Instruction};
use crate::{Context, ObjectInstance};

#[derive(Clone)]
pub struct Return {
    value: Option<Box<dyn Instruction>>,
}

impl Return {
    /// Create a new IfElse block and return it
    pub fn new(value: Option<Box<dyn Instruction>>) -> Return {
        Return { value }
    }
}

impl Instruction for Return {
    fn kind(&self) -> InstrKind {
        match &self.value {
            Some(val) => val.kind(),
            None => InstrKind::Statement,
        }
    }

    fn print(&self) -> String {
        let base = "return".to_string();

        match &self.value {
            Some(val) => format!("{} {}", base, val.print()),
            None => base,
        }
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        match &self.value {
            Some(val) => val.execute(ctx),
            None => None,
        }
    }
}
