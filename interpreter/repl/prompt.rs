//! Creates a prompt based on the context's current status

use colored::Colorize;
use jinko::Context;

pub struct Prompt;

impl Prompt {
    /// Create the prompt based on the actual context conditions
    pub fn get(_: &Context) -> String {
        format!("jinko {} ", ">".purple())
    }
}
