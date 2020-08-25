//! The broccoli interpreter keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains a information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns an "Interpreter", which is really just a complex structure
//! aggregating the necessary information to run a broccoli program.

mod scope_map;
use scope_map::ScopeMap;

use std::collections::HashMap;

use crate::instruction::{FunctionDec, Instruction, Var};

/// Type the interpreter uses for keys
type IKey = String;

/// Name of the entry point in broccoli
const ENTRY_NAME: &str = "__entry";

pub struct Interpreter {
    /// Is the interpreter in an audit block or not
    pub in_audit: bool,

    /// Entry point to the interpreter, the "main" function
    pub entry_point: FunctionDec,

    /// Functions registered in the interpreter
    functions: HashMap<IKey, FunctionDec>,
    /// Variables registered in the interpreter
    variables: HashMap<IKey, Var>,

    /// Tests registered in the interpreter
    tests: HashMap<IKey, FunctionDec>,
    /// External functions registered in the interpreter
    exts: HashMap<IKey, FunctionDec>,
}

impl Interpreter {
    /// Create a new empty interpreter. Starts in non-audit mode
    pub fn new() -> Interpreter {
        Interpreter {
            in_audit: false,

            entry_point: FunctionDec::new(String::from(ENTRY_NAME), None),
            functions: HashMap::new(),
            variables: HashMap::new(),

            tests: HashMap::new(),
            exts: HashMap::new(),
        }
    }

    /// Add a function to the interpreter. Returns `Ok` if the function was added, `Err`
    /// if it existed already and was not.
    // FIXME: Add semantics error type
    pub fn add_function(&mut self, function: FunctionDec) -> Result<(), String> {
        match self.functions.get(function.name()) {
            Some(_) => Err(format!("function already declared: {}", function.name())),
            None => Ok({
                self.functions.insert(function.name().to_owned(), function);
            }),
        }
    }

    /// Add a variable to the interpreter. Returns `Ok` if the variable was added, `Err`
    /// if it existed already and was not.
    // FIXME: Add semantics error type
    pub fn add_variable(&mut self, var: Var) -> Result<(), String> {
        match self.variables.get(var.name()) {
            Some(_) => Err(format!("variable already declared: {}", var.name())),
            None => Ok({
                self.variables.insert(var.name().to_owned(), var);
            }),
        }
    }

    /// Pretty-prints valid broccoli code from a given interpreter
    pub fn print(&self) -> String {
        self.entry_point.print()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn t_redefinition_of_function() {
        let f0 = FunctionDec::new("f0".to_owned(), None);
        let f0_copy = FunctionDec::new("f0".to_owned(), None);

        let mut i = Interpreter::new();

        assert_eq!(i.add_function(f0), Ok(()));
        assert_eq!(
            i.add_function(f0_copy),
            Err("function already declared: f0".to_owned())
        );
    }

    #[test]
    fn t_redefinition_of_variable() {
        let v0 = Var::new("v0".to_owned());
        let v0_copy = Var::new("v0".to_owned());

        let mut i = Interpreter::new();

        assert_eq!(i.add_variable(v0), Ok(()));
        assert_eq!(
            i.add_variable(v0_copy),
            Err("variable already declared: v0".to_owned())
        );
    }
}
