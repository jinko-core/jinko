//! A `ScopeMap` is a set of "scopes" used to keep track of the available variables and
//! functions in a given scope.
//! In order to access variables and functions, the scope map first looks in the current
//! scope. If the specified name cannot be found, it searches the other scopes, defined
//! before the current one, until it finds the correct component.

use std::collections::{HashMap, LinkedList};

use crate::{ErrKind, Error};

/// A scope contains a set of available variables, functions and types
#[derive(Clone)]
pub struct Scope<V, F, T> {
    pub(crate) variables: HashMap<String, V>,
    pub(crate) functions: HashMap<String, F>,
    pub(crate) types: HashMap<String, T>,
}

impl<V, F, T> Scope<V, F, T> {
    /// Get a reference on a variable from the scope map if is has been inserted already
    pub fn get_variable(&self, name: &str) -> Option<&V> {
        self.variables.get(name)
    }

    /// Get a mutable reference on a variable from the scope map if it has been inserted already
    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut V> {
        self.variables.get_mut(name)
    }

    /// Get a reference on a function from the scope map if is has been inserted already
    pub fn get_function(&self, name: &str) -> Option<&F> {
        self.functions.get(name)
    }

    /// Get a reference on a type from the scope map if is has been inserted already
    pub fn get_type(&self, name: &str) -> Option<&T> {
        self.types.get(name)
    }

    /// Add a variable to the most recently created scope, if it doesn't already exist
    pub fn add_variable(&mut self, name: String, var: V) -> Result<(), Error> {
        match self.get_variable(&name) {
            Some(_) => Err(Error::new(ErrKind::Context)
                .with_msg(format!("variable already declared: {}", name))),
            None => {
                self.variables.insert(name, var);
                Ok(())
            }
        }
    }

    /// Remove a variable from the most recently created scope, if it exists
    pub fn remove_variable(&mut self, name: &str) -> Result<(), Error> {
        match self.get_variable(name) {
            Some(_) => {
                self.variables.remove(name).unwrap();
                Ok(())
            }
            None => {
                Err(Error::new(ErrKind::Context)
                    .with_msg(format!("variable does not exist: {}", name)))
            }
        }
    }

    /// Add a variable to the most recently created scope, if it doesn't already exist
    pub fn add_function(&mut self, name: String, func: F) -> Result<(), Error> {
        match self.get_function(&name) {
            Some(_) => Err(Error::new(ErrKind::Context)
                .with_msg(format!("function already declared: {}", name))),
            None => {
                self.functions.insert(name, func);
                Ok(())
            }
        }
    }

    /// Add a type to the most recently created scope, if it doesn't already exist
    pub fn add_type(&mut self, name: String, type_dec: T) -> Result<(), Error> {
        match self.get_type(&name) {
            Some(_) => {
                Err(Error::new(ErrKind::Context)
                    .with_msg(format!("type already declared: {}", name)))
            }
            None => {
                self.types.insert(name, type_dec);
                Ok(())
            }
        }
    }
}

impl<V, F, T> Default for Scope<V, F, T> {
    /// Create a new empty Scope
    fn default() -> Scope<V, F, T> {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
            types: HashMap::new(),
        }
    }
}

/// A scope stack is a reversed stack. This alias is made for code clarity
pub type ScopeStack<T> = LinkedList<T>;

/// A scope map keeps track of the currently available scopes and the current depth
/// level.
#[derive(Clone, Default)]
pub struct ScopeMap<V, F, T> {
    scopes: ScopeStack<Scope<V, F, T>>,
}

impl<V, F, T> ScopeMap<V, F, T> {
    /// Create a new empty scope map, at depth 0
    pub fn new() -> ScopeMap<V, F, T> {
        ScopeMap {
            scopes: ScopeStack::new(),
        }
    }

    /// Get a reference on the scopes inside a ScopeMap
    pub fn scopes(&self) -> &ScopeStack<Scope<V, F, T>> {
        &self.scopes
    }

    /// Enter into a new scope
    pub fn scope_enter(&mut self) {
        self.scopes.push_front(Scope::default());
    }

    /// Exit the last added scope
    pub fn scope_exit(&mut self) {
        // We unwrap since we want the context to crash in case we pop an unexisting
        // scope.
        self.scopes.pop_front().unwrap();
    }

    /// Maybe get a variable in any available scopes
    pub fn get_variable(&self, name: &str) -> Option<&V> {
        // FIXME: Use find for code quality?
        for scope in self.scopes.iter_mut() {
            match scope.get_variable(name) {
                Some(v) => return Some(v),
                None => continue,
            };
        }

        None
    }

    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut V> {
        for scope in self.scopes.iter_mut() {
            match scope.get_variable_mut(name) {
                Some(v) => return Some(v),
                None => continue,
            };
        }

        None
    }

    /// Maybe get a function in any available scopes
    pub fn get_function(&self, name: &str) -> Option<&F> {
        // FIXME: Use find for code quality?
        for scope in self.scopes.iter() {
            match scope.get_function(name) {
                Some(v) => return Some(v),
                None => continue,
            };
        }

        None
    }

    /// Maybe get a type in any available scopes
    pub fn get_type(&self, name: &str) -> Option<&T> {
        // FIXME: Use find for code quality?
        for scope in self.scopes.iter() {
            match scope.get_type(name) {
                Some(v) => return Some(v),
                None => continue,
            };
        }

        None
    }

    /// Add a variable to the current scope if it hasn't been added before
    pub fn add_variable(&mut self, name: String, var: V) -> Result<(), Error> {
        match self.scopes.front_mut() {
            Some(head) => head.add_variable(name, var),
            None => Err(Error::new(ErrKind::Context)
                .with_msg(String::from("Adding variable to empty scopemap"))),
        }
    }

    /// Remove a variable from the current scope if it hasn't been added before
    pub fn remove_variable(&mut self, name: &str) -> Result<(), Error> {
        match self.scopes.front_mut() {
            Some(head) => head.remove_variable(name),
            None => Err(Error::new(ErrKind::Context)
                .with_msg(String::from("Removing variable from empty scopemap"))),
        }
    }

    /// Add a function to the current scope if it hasn't been added before
    pub fn add_function(&mut self, name: String, func: F) -> Result<(), Error> {
        match self.scopes.front_mut() {
            Some(head) => head.add_function(name, func),
            None => Err(Error::new(ErrKind::Context)
                .with_msg(String::from("Adding function to empty scopemap"))),
        }
    }

    /// Add a type to the current scope if it hasn't been added before
    pub fn add_type(&mut self, name: String, custom_type: T) -> Result<(), Error> {
        match self.scopes.front_mut() {
            Some(head) => head.add_type(name, custom_type),
            None => Err(Error::new(ErrKind::Context)
                .with_msg(String::from("Adding new custom type to empty scopemap"))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::Var;

    macro_rules! s {
        ($s:literal) => {
            String::from($s)
        };
    }

    fn new_scopemap() -> ScopeMap<Var, (), ()> {
        ScopeMap::new()
    }

    #[test]
    #[should_panic]
    fn t_pop_non_existent_scope() {
        let mut s = new_scopemap();

        s.scope_enter();
        s.scope_exit();
        s.scope_exit();
    }

    #[test]
    #[should_panic]
    fn t_add_var_non_existent_scope() {
        let mut s = new_scopemap();

        s.add_variable(s!("Something"), Var::new("Something".to_owned()))
            .unwrap();
    }

    #[test]
    fn t_find_non_existent_var() {
        let s = new_scopemap();

        assert!(s.get_variable("a").is_none());
    }

    #[test]
    fn t_add_var_and_get_it() {
        let mut s = new_scopemap();

        s.scope_enter();
        s.add_variable(s!("a"), Var::new("a".to_owned())).unwrap();

        assert!(s.get_variable("a").is_some());
    }

    #[test]
    fn t_add_var_and_get_it_from_inner_scope() {
        let mut s = new_scopemap();

        s.scope_enter();
        s.add_variable(s!("a"), Var::new("a".to_owned())).unwrap();

        s.scope_enter();
        s.scope_enter();
        s.scope_enter();
        s.scope_enter();
        s.scope_enter();

        assert!(s.get_variable("a").is_some());
    }

    #[test]
    fn t_add_var_and_get_it_from_outer_scope() {
        let mut s = new_scopemap();

        s.scope_enter();

        s.add_variable(s!("a"), Var::new("a".to_owned())).unwrap();

        s.scope_exit();

        assert!(s.get_variable("a").is_none());
    }

    #[test]
    fn t_scope_of_anything() {
        let _ = Scope::<i32, i32, String>::default();
        let _ = Scope::<(), (), ()>::default();
    }
}
