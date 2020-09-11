//! The jinko interpreter keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains a information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns an "Interpreter", which is really just a complex structure
//! aggregating the necessary information to run a jinko program.

mod scope_map;
use scope_map::ScopeMap;

use std::collections::HashMap;
use std::rc::Rc;

use crate::error::JinkoError;
use crate::instruction::{Block, FunctionDec, Instruction, Var};

/// Type the interpreter uses for keys
type IKey = String;

/// Name of the entry point in jinko
const ENTRY_NAME: &str = "__entry";

pub struct Interpreter {
    /// Is the interpreter in an audit block or not
    pub in_audit: bool,

    /// Entry point to the interpreter, the "main" function
    pub entry_point: FunctionDec,

    /// Contains the scopes of the interpreter, in which are variables and functions
    scope_map: ScopeMap,

    /// Tests registered in the interpreter
    tests: HashMap<IKey, FunctionDec>,
    /// External functions registered in the interpreter
    exts: HashMap<IKey, FunctionDec>,
}

impl Interpreter {
    /// Create a new empty interpreter. Starts in non-audit mode
    pub fn new() -> Interpreter {
        let mut i = Interpreter {
            in_audit: false,

            entry_point: FunctionDec::new(String::from(ENTRY_NAME), None),
            scope_map: ScopeMap::new(),

            tests: HashMap::new(),
            exts: HashMap::new(),
        };

        i.entry_point.set_block(Block::new());

        // FIXME: Necessary?
        i.scope_enter();

        i
    }

    /// Add a function to the interpreter. Returns `Ok` if the function was added, `Err`
    /// if it existed already and was not.
    // FIXME: Add semantics error type
    pub fn add_function(&mut self, function: FunctionDec) -> Result<(), JinkoError> {
        self.scope_map.add_function(function)
    }

    /// Add a variable to the interpreter. Returns `Ok` if the variable was added, `Err`
    /// if it existed already and was not.
    // FIXME: Add semantics error type
    pub fn add_variable(&mut self, var: Var) -> Result<(), JinkoError> {
        self.scope_map.add_variable(var)
    }

    /// Get a mutable reference on an existing function
    pub fn get_function(&self, name: &str) -> Option<&Rc<FunctionDec>> {
        self.scope_map.get_function(name)
    }

    /// Get a reference on an existing variable
    pub fn get_variable(&self, name: &str) -> Option<&Var> {
        self.scope_map.get_variable(name)
    }

    /// Create a new empty scope
    pub fn scope_enter(&mut self) {
        self.scope_map.scope_enter()
    }

    /// Exit the latest created scope
    pub fn scope_exit(&mut self) {
        self.scope_map.scope_exit()
    }

    /// Enter audit mode
    pub fn audit_enter(&mut self) {
        self.in_audit = true;
    }

    /// Exit audit mode
    pub fn audit_exit(&mut self) {
        self.in_audit = false;
    }

    /// Run the interpreter once, altering its contents
    pub fn run_once(&mut self) {
        // Take ownership of the entry point, replacing it with a new one,
        // and execute it
        match std::mem::take(&mut self.entry_point).execute(self) {
            Ok(_) => {}
            Err(e) => e.exit(),
        }
    }

    /// Pretty-prints valid jinko code from a given interpreter
    pub fn print(&self) -> String {
        self.entry_point.print()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::error::ErrKind;

    #[test]
    fn t_redefinition_of_function() {
        let f0 = FunctionDec::new("f0".to_owned(), None);
        let f0_copy = FunctionDec::new("f0".to_owned(), None);

        let mut i = Interpreter::new();

        assert_eq!(i.add_function(f0), Ok(()));
        assert_eq!(
            i.add_function(f0_copy).err().unwrap().kind(),
            ErrKind::Interpreter,
        );
    }

    #[test]
    fn t_redefinition_of_variable() {
        let v0 = Var::new("v0".to_owned());
        let v0_copy = Var::new("v0".to_owned());

        let mut i = Interpreter::new();

        assert_eq!(i.add_variable(v0), Ok(()));
        assert_eq!(
            i.add_variable(v0_copy).err().unwrap().kind(),
            ErrKind::Interpreter,
        );
    }
}
