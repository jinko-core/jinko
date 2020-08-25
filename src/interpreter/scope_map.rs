//! A `ScopeMap` is a set of "scopes" used to keep track of the available variables and
//! functions in a given scope.
//! In order to access variables and functions, the scope map first looks in the current
//! scope. If the specified name cannot be found, it searches the other scopes, defined
//! before the current one, until it finds the correct component.

use std::collections::{HashMap, LinkedList};

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

/// A scope stack is a reversed stack. This alias is made for code clarity
type ScopeStack<T> = LinkedList<T>;

/// A scope map keeps track of the currently available scopes and the current depth
/// level.
pub struct ScopeMap {
    scopes: ScopeStack<Scope>,
}

impl ScopeMap {
    /// Create a new empty scope map, at depth 0
    pub fn new() -> ScopeMap {
        ScopeMap {
            scopes: ScopeStack::new(),
        }
    }

    /// Enter into a new scope
    pub fn scope_enter(&mut self) {
        self.scopes.push_front(Scope::new());
    }

    /// Exit the last added scope
    pub fn scope_exit(&mut self) {
        // We unwrap since we want the interpreter to crash in case we pop an unexisting
        // scope.
        self.scopes.pop_front().unwrap();
    }

    pub fn get_variable(&self, name: &str) -> Option<&Var> {
        for scope in self.scopes.iter() {
        }

        todo!()
    }

    pub fn get_function(&self, name: &str) -> Option<&FunctionDec> {
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
