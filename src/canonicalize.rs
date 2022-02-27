//! First pass of the interpreter. This builds a [`ScopeMap`], instruction by instruction,
//! resolving names eagerly as they go. If a name cannot be resolved, then it is an undeclared
//! error.

use crate::ScopeMap;

use std::sync::atomic::{AtomicU64, Ordering};

fn create_anonymous_path() -> String {
    static ANONYMOUS_PATH_COUNTER: AtomicU64 = AtomicU64::new(0);

    let anon_path_count = ANONYMOUS_PATH_COUNTER.fetch_add(1, Ordering::SeqCst);

    format!("<anonymous_{}>", anon_path_count)
}

pub trait Canonicalize: Clone {
    /// Returns the current name of the [`Instruction`], before it gets
    /// canonicalized.
    ///
    /// ```ignore
    /// // Say we have the current code
    /// func outer() {
    ///     func inner() {
    ///         func innermost() {
    ///             // This function is nested: its canonicalize name should
    ///             // be outer::inner::innermost
    ///         }
    ///     }
    /// }
    /// ```
    ///
    /// Calling `current_name()` on the node associated with the `innermost`
    /// function declaration should return `innermost`, not its canonicalized
    /// name.
    fn current_name(&self) -> &str;

    /// Set the name of an [`Instruction`]. This is used so that we can
    /// implement `canonicalize` directly through the trait, without having
    /// to rely on every [`Instruction`] implementing it.
    fn set_name(&mut self, name: String);

    /// Name separator used when canonicalizing paths
    const SEPARATOR: &'static str = "@";

    /// Return a copy of self with a canonicalized name
    // FIXME: We probably need to Box<Self> right?
    fn canonicalize(&self, current_path: &str) -> Self {
        let mut new_self = self.clone();

        // If the path is empty, then no need for a separator. We're at the
        // beginning of an outermost scope
        if !current_path.is_empty() {
            let old_name = self.current_name();
            new_self.set_name(format!("{}{}{}", current_path, Self::SEPARATOR, old_name));
        }

        new_self
    }

    /// Visitor-like function to build a [`ScopeMap`] instruction by instruction.
    /// An [`Instruction`] implementing this needs to canonicalize, add itself
    /// to the scope map in a proper way, and then visits its children with
    /// its new canonicalized name.
    fn build_scope_map(&self, scope_map: ScopeMap, current_path: &str) -> ScopeMap;
}
