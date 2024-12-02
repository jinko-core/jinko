//! A `ScopeMap` is a set of "scopes" used to keep track of the available variables and
//! functions in a given scope.
//! In order to access variables and functions, the scope map first looks in the current
//! scope. If the specified name cannot be found, it searches the other scopes, defined
//! before the current one, until it finds the correct component.

use std::{
    borrow::Borrow,
    collections::{HashMap, LinkedList},
    hash::Hash,
};

use crate::error::{ErrKind, Error};

/// A scope contains a set of available variables, functions and types.
// FIXME: Shoud we split this in two? Between this and an ExecutionScope type?
#[derive(Clone)]
pub struct Scope<V, F, T> {
    /// Variables cannot be generic: They always have a concrete well defined
    /// type.
    pub(crate) variables: HashMap<String, V>,
    /// There are two types of functions: "base", generic functions, which can
    /// be duplicated and specialized into multiple versions, and "final" functions
    /// which are already specialized (or do not contain generics in the first place).
    /// Specialized functions should be referenced by their mangled names. If they
    /// do not contain generics, then they will simply be referenced by their
    /// names.
    pub(crate) generic_functions: HashMap<String, F>,
    pub(crate) functions: HashMap<String, F>,
    /// Similarly, there are two types of types: generic types and specialized types, which are final.
    /// Types are identified by their [`TypeId`]. There is no way to differentiate
    /// between a generic [`TypeId`] and a specialized one, so you must be careful
    /// when appending types to the map.
    pub(crate) generic_types: HashMap<String, T>,
    pub(crate) types: HashMap<String, T>,
}

impl<V, F, T> Default for Scope<V, F, T> {
    /// Create a new empty Scope
    fn default() -> Scope<V, F, T> {
        Scope {
            variables: HashMap::new(),
            generic_functions: HashMap::new(),
            functions: HashMap::new(),
            generic_types: HashMap::new(),
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

    fn get<'map, K, Q, U>(
        &'map self,
        key: &Q,
        map_extractor: impl Fn(&Scope<V, F, T>) -> &HashMap<K, U>,
    ) -> Option<&'map U>
    where
        K: Borrow<Q> + Hash + Eq + 'map,
        Q: Hash + Eq + ?Sized,
    {
        self.scopes()
            .iter()
            .map(|scope| map_extractor(scope).get(key))
            .find(|var| var.is_some())?
    }

    fn insert_unique<K, U>(
        &mut self,
        key: K,
        value: U,
        map_extractor: impl Fn(&mut Scope<V, F, T>) -> &mut HashMap<K, U>,
    ) -> Result<(), Error>
    where
        K: Hash + Eq,
    {
        // If there is no front scope, this is an error in the interpreter's
        // logic
        let top = self.scopes.front_mut().unwrap();
        let map = map_extractor(top);

        match map.get(&key) {
            Some(_) => Err(Error::new(ErrKind::Context)),
            None => {
                map.insert(key, value);
                Ok(())
            }
        }
    }

    /// Maybe get a variable in any available scopes
    pub fn get_variable(&self, name: &str) -> Option<&V> {
        self.get(name, |scope| &scope.variables)
    }

    /// Maybe get a mutable reference to a variable in any available scopes
    pub fn get_variable_mut(&mut self, name: &str) -> Option<&mut V> {
        self.scopes
            .iter_mut()
            .map(|scope| scope.variables.get_mut(name))
            .find(|var| var.is_some())?
    }

    /// Maybe get a function in any available scopes
    pub fn get_function(&self, name: &str) -> Option<&F> {
        self.get(name, |scope| &scope.functions)
    }

    /// Maybe get a generic function in any available scopes
    pub fn get_generic_function(&self, name: &str) -> Option<&F> {
        self.get(name, |scope| &scope.generic_functions)
    }

    /// Maybe get a type in any available scopes
    pub fn get_type(&self, name: &str) -> Option<&T> {
        self.get(name, |scope| &scope.types)
    }

    /// Maybe get a generic type in any available scopes
    pub fn get_generic_type(&self, name: &str) -> Option<&T> {
        self.get(name, |scope| &scope.generic_types)
    }

    /// Add a variable to the current scope if it hasn't been added before
    pub fn add_variable(&mut self, name: String, var: V) -> Result<(), Error> {
        self.insert_unique(name, var, |scope| &mut scope.variables)
    }

    /// Add a function to the current scope if it hasn't been added before
    pub fn add_function(&mut self, name: String, func: F) -> Result<(), Error> {
        self.insert_unique(name, func, |scope| &mut scope.functions)
    }

    /// Add a generic function to the current scope if it hasn't been added before
    pub fn add_generic_function(&mut self, name: String, func: F) -> Result<(), Error> {
        self.insert_unique(name, func, |scope| &mut scope.generic_functions)
    }

    /// Add a type to the current scope if it hasn't been added before
    pub fn add_type(&mut self, name: String, custom_type: T) -> Result<(), Error> {
        self.insert_unique(name, custom_type, |scope| &mut scope.types)
    }

    /// Add a generic type to the current scope if it hasn't been added before
    pub fn add_generic_type(&mut self, name: String, custom_type: T) -> Result<(), Error> {
        self.insert_unique(name, custom_type, |scope| &mut scope.generic_types)
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
