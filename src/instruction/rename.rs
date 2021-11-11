//! The Rename trait should be implemented for every type that implements the Instruction
//! trait. However, some types need to be renamed without being instructions, hence the
//! existence of this trait.
//!
//! This trait exists because some structures need to be renamed. For example, when
//! including a new source in jinko code, a namespace is added at the front of identifiers.
//! If some variable `a` is declared in the `module` source file, then it becomes
//! `module::a` upon inclusion. In the same vein, types and function declarations change,
//! so type instanciations and function calls must also change.
//!
//! Some Rust structures need to be renamable without being instructions, such as `DecArg`s.
//! Indeed, a `DecArg` refers to a type in its current source or relative to it, which
//! will change once that source is itself included. However, `DecArg`s are just helper
//! structures and are not instructions. They cannot be executed or optimized.
//!
//! For some simple constructs, this simply means renaming self's name, such as a TypeDec:
//!
//! ```ignore
//! // some_type.jk
//! type SomeType(...);
//!
//! // main.jk
//! incl some_type
//!
//! a = some_type::SomeType(...);
//!
//! // SomeType is now called `some_type::SomeType`. It has been prefixed
//! ```
//!
//! Other instructions also have owernship of other instructions, such as blocks:
//! ```ignore
//! // source.jk
//! { // block enter
//!     type InBlock(...);
//!     block_var = 15;
//! }
//! ```
//! If we rename that block, then the TypeDec and VarAssign both need to prefixed
//! as well. Much like the Instruction trait, every function should also be called on
//! fields implementing the Rename trait when being executed, allowing for an AST-like
//! behavior

// FIXME: This trait needs to be worked on and fixed. For now, do not implement it.
// pub trait Rename {
//     /// Add a prefix to self and its possible components
//     fn prefix(&mut self, _prefix: &str) {}
// }
