//! Statements are instructions that return `Nothing` upon execution

#[derive(Clone, Copy)]
pub struct Statement;

use super::InstrTrait;
use super::ReturnKind;

// FIXME: Careful of error handling ? What happens when a statement actually returns
// something ?

/// Statements always return `Nothing`
impl InstrTrait for Statement {
    fn execute(&self) -> Result<ReturnKind, String> {
        Ok(ReturnKind::Nothing)
    }
}
