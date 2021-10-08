//! This module contains all builtin functions declared in the jinko interpreter

use std::collections::HashMap;
use crate::{Context, Instruction, JkInt, JkString, ObjectInstance};
use crate::instance::{ToObjectInstance, FromObjectInstance};

type Args = Vec<Box<dyn Instruction>>;
type BuiltinFn = fn(&mut Context, Args) -> Option<ObjectInstance>;

/// Contains the various components declared during the interpreter's initialization
pub struct Builtins {
    functions: HashMap<String, BuiltinFn>,
}

impl Builtins {
    /// Get the length of a string
    /// Defined in stdlib/string.jk
    /// The first argument is the string to get the length of
    ///
    /// # Example
    /// ```
    /// s.len()
    /// len("hey")
    /// "hey".len()
    /// ```
    fn string_len(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
        let arg0 = args[0].execute(ctx).unwrap();
        let jk_string = JkString::from_instance(&arg0);

        Some(JkInt::from(jk_string.0.len() as i64).to_instance())
    }

    fn add(&mut self, name: &'static str, builtin_fn: BuiltinFn) {
        self.functions.insert(String::from(name), builtin_fn);
    }

    /// Create a new instance of builtins, with pre-defined functions
    pub fn new() -> Builtins {
        let mut builtins = Builtins {
            functions: HashMap::new(),
        };

        builtins.add("len", Builtins::string_len);

        builtins
    }
}
