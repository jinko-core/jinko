// FIXME: Make crate attribute `#![warn(missing_docs)]`

pub mod builtins;
pub mod context;
pub mod error;
#[cfg(feature = "ffi")]
mod ffi;
pub mod generics;
mod indent;
pub mod instance;
pub mod instruction;
pub mod location;
pub mod log;
pub mod parser;
pub mod symbol;
pub mod typechecker;
mod utils;
pub mod value;
