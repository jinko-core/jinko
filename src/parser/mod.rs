//! The broccoli parser transforms user inputs into a map of instructions. A special
//! entry is created for the "main" function of the program. Including modules adds
//! instructions to that main entry.

use super::function::Function;
use std::collections::HashMap;

mod tokens;

use tokens::Token;

pub struct Parser;

impl Parser {
    /// Parses the entire user input and returns a hashmap corresponding to the user
    /// program
    pub fn parse(input: &str) -> Result<HashMap<String, Function>, String> {
        todo!()
    }
}
