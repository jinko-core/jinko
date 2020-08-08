//! Statements are instructions that return `Nothing` upon execution

use super::InstrTrait;
use super::ReturnKind;

#[derive(Clone, Copy)]
pub struct Statement;

// FIXME: Careful of error handling ? What happens when a statement actually returns
// something ?

/// Statements always return `Nothing`
impl InstrTrait for Statement {
    fn execute(&self) -> Result<ReturnKind, String> {
        Ok(ReturnKind::Nothing)
    }
}
