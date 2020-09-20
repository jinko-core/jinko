//! The jinko interpreter keeps track of variables and functions, and dispatches calls
//! and references to their correct location. It contains a information regarding the
//! registered functions and variables. It also handles garbage collection. Parsing a
//! source file returns an "Interpreter", which is really just a complex structure
//! aggregating the necessary information to run a jinko program.

use colored::Colorize;

mod scope_map;
use scope_map::ScopeMap;

use std::collections::HashMap;
use std::rc::Rc;

use crate::error::JinkoError;
use crate::instruction::{Block, FunctionDec, FunctionKind, Instruction, Var};

/// Type the interpreter uses for keys
type IKey = String;

/// Name of the entry point in jinko
const ENTRY_NAME: &str = "__entry";

pub struct Interpreter {
    /// Is the interpreter in an audit block or not
    pub in_audit: bool,

    /// Is the interpreter in debugging mode or not
    pub debug_mode: bool,

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
    // FIXME: Remove this function
    fn new_entry() -> FunctionDec {
        let mut ep = FunctionDec::new(String::from(ENTRY_NAME), None);

        ep.set_kind(FunctionKind::Func);
        ep.set_block(Block::new());

        ep
    }

    /// Create a new empty interpreter. Starts in non-audit mode
    pub fn new() -> Interpreter {
        let mut i = Interpreter {
            in_audit: false,
            debug_mode: false,

            entry_point: Self::new_entry(),
            scope_map: ScopeMap::new(),

            tests: HashMap::new(),
            exts: HashMap::new(),
        };

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

    /// Pretty-prints valid jinko code from a given interpreter
    pub fn print(&self) -> String {
        self.entry_point.print()
    }

    /// Print a debug message if the interpreter is in debug mode, according to the
    /// following format:
    ///
    /// `<specifier>: <msg>`
    pub fn debug(&self, specifier: &str, msg: &str) {
        if self.debug_mode {
            println!("{}: {}", specifier.purple(), msg);
        }
    }

    /// Print a debugging step if the interpreter is in debug mode, according to the
    /// following format:
    ///
    /// `<specifier>`
    ///
    /// This is used to indicate "steps" the interpreter is taking where adding a
    /// secondary format is not necesarry. For example, when entering a block: There's
    /// no way to name a block, so no necessity to have more information other than
    /// "ENTER_BLOCK"
    pub fn debug_step(&self, specifier: &str) {
        if self.debug_mode {
            println!("{}", specifier.yellow());
        }
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
