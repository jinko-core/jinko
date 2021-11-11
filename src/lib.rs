// FIXME: Make crate attribute `#![warn(missing_docs)]`

mod builtins;
mod context;
mod error;
mod ffi;
mod indent;
mod instance;
mod instruction;
mod parser;
mod typechecker;
mod utils;
mod value;

pub use builtins::Builtins;
pub use context::{Context, Scope, ScopeMap};
pub use error::{ErrKind, Error};
pub use indent::Indent;
pub use instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
pub use instruction::{InstrKind, Instruction};
pub use parser::{parse, Construct};
pub use typechecker::{CheckedType, TypeCheck, TypeCtx};
pub use value::{JkBool, JkChar, JkConstant, JkFloat, JkInt, JkString, Value};
