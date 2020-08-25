//! A `ScopeMap` is a set of "scopes" used to keep track of the available variables and
//! functions in a given scope.

use std::collections::HashMap;

use crate::instruction::{Var, FunctionDec};

/// A scope contains a set of available variables and functions
struct Scope {
    variables: HashMap<String, Var>,
    functions: HashMap<String, FunctionDec>,
}

impl Scope {
    /// Create a new empty Scope
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

/// A stack is simply a vector. This alias is made for code clarity
type ScopeStack<T> = Vec<T>;

/// A scope map keeps track of the currently available scopes and the current depth
/// level.
pub struct ScopeMap {
    depth: usize,
    scopes: ScopeStack<Scope>,
}

impl ScopeMap {
    /// Create a new empty scope map, at depth 0
    pub fn new() -> ScopeMap {
        ScopeMap {
            depth: 0usize,
            scopes: ScopeStack::new(),
        }
    }

    /// Enter into a new scope
    pub fn scope_enter(&mut self) {
        self.scopes.push(Scope::new())
    }

    /// Exit the last added scope
    pub fn scope_exit(&mut self) {
        // We unwrap since we want the interpreter to crash in case we pop an unexisting
        // scope.
        self.scopes.pop().unwrap();
    }

    pub fn get_variable(&self, name: &str) -> &Var {
        todo!()
    }

    pub fn get_function(&self, name: &str) -> &FunctionDec {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn t_pop_non_existent_scope() {
        let mut s = ScopeMap::new();

        s.scope_enter();
        s.scope_exit();
        s.scope_exit();
    }
}
