// FIXME: Make crate attribute `#![warn(missing_docs)]`

pub mod builtins;
pub mod context;
pub mod debug;
pub mod error;
#[cfg(feature = "ffi")]
mod ffi;
// FIXME: Re-add once we reimplement generics
// pub mod generics;
mod indent;
pub mod instance;
pub mod instruction;
pub mod io_trait;
pub mod parser;
pub mod typechecker;
mod utils;
pub mod value;

pub use crate::error::{ErrKind, Error};
pub use builtins::Builtins;
pub use context::{Context, Scope, ScopeMap};
// FIXME: Re-add once we reimplement generics
// pub use generics::GenericUser;
pub use indent::Indent;
pub use instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
pub use instruction::{InstrKind, Instruction};
pub use io_trait::JkReader;
pub use location::{self, Location, SpanTuple};
pub use parser::{constructs, parse, ParseInput};
pub use symbol;
pub use typechecker::{CheckedType, TypeCheck, TypeCtx, TypeId};
pub use value::{JkBool, JkChar, JkConstant, JkFloat, JkInt, JkString, Value};
