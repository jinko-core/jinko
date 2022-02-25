// FIXME: Make crate attribute `#![warn(missing_docs)]`

pub mod builtins;
mod context;
mod error;
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
mod value;

pub use builtins::Builtins;
pub use context::{Context, Scope, ScopeMap};
pub use error::{ErrKind, Error};
pub use generics::Generic;
pub use indent::Indent;
pub use instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
pub use instruction::{InstrKind, Instruction};
pub use location::{Location, SpanTuple};
pub use parser::{constructs, parse, ParseInput};
pub use typechecker::{CheckedType, TypeCheck, TypeCtx, TypeId};
pub use value::{JkBool, JkChar, JkConstant, JkFloat, JkInt, JkString, Value};
