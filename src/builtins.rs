//! This module contains all builtin functions declared in the jinko interpreter

use std::collections::HashMap;
use std::path::PathBuf;

use crate::context::Context;
#[cfg(feature = "ffi")]
use crate::ffi;
use crate::instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
use crate::instruction::Instruction;
use crate::value::{JkBool, JkChar, JkFloat, JkInt, JkString};

type Args = Vec<Box<dyn Instruction>>;
type BuiltinFn = fn(&mut Context, Args) -> Option<ObjectInstance>;

/// Contains the various components declared during the interpreter's initialization
pub struct Builtins {
    functions: HashMap<String, BuiltinFn>,
}

/// Get the length of a string. Defined in stdlib/string.jk
/// The first argument is the string to get the length of
fn string_len(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let arg0 = args[0].execute(ctx).unwrap();
    let jk_string = JkString::from_instance(&arg0);

    Some(JkInt::from(jk_string.0.len() as i64).to_instance())
}

/// Concatenate two strings together. Defined in stdlib/string.jk
fn string_concat(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let lhs = JkString::from_instance(&args[0].execute(ctx).unwrap()).0;
    let rhs = JkString::from_instance(&args[1].execute(ctx).unwrap()).0;

    Some(JkString::from(format!("{}{}", lhs, rhs)).to_instance())
}

fn string_display(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let s = JkString::from_instance(&args[0].execute(ctx).unwrap()).0;
    let add_newline = JkBool::from_instance(&args[1].execute(ctx).unwrap()).0;

    print!("{}", s);

    if add_newline {
        println!()
    }

    None
}

fn string_display_err(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let s = JkString::from_instance(&args[0].execute(ctx).unwrap()).0;
    let add_newline = JkBool::from_instance(&args[1].execute(ctx).unwrap()).0;

    eprint!("{}", s);

    if add_newline {
        eprintln!()
    }

    None
}

fn string_is_empty(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let s = JkString::from_instance(&args[0].execute(ctx).unwrap()).0;

    Some(JkBool::from(s.is_empty()).to_instance())
}

fn string_equals(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let lhs = JkString::from_instance(&args[0].execute(ctx).unwrap()).0;
    let rhs = JkString::from_instance(&args[1].execute(ctx).unwrap()).0;

    Some(JkBool::from(lhs == rhs).to_instance())
}

/// Link with a given library at runtime
fn ffi_link_with(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    #[cfg(feature = "ffi")]
    {
        let lib_path = JkString::from_instance(&args[0].execute(ctx).unwrap()).0;

        if let Err(e) = ffi::link_with(ctx, PathBuf::from(&lib_path)) {
            ctx.error(e.with_msg(format!("couldn't link with library `{}`", &lib_path)));
        }

        None
    }

    #[cfg(not(feature = "ffi"))]
    {
        use crate::{ErrKind, Error};

        ctx.error(Error::new(ErrKind::Context).with_msg(format!(
            "jinko is not compiled with FFI support. `link_with()` is disabled"
        )));

        None
    }
}

// Get an argument from the argument vector at a certain index
fn arg_get(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let idx = JkInt::from_instance(&args[0].execute(ctx).unwrap()).0;

    let args = ctx.args();

    let result_string = match idx {
        0 => ctx.path().map_or(std::env::args().next().unwrap(), |s| {
            s.to_str().unwrap().to_string()
        }),
        _ => args
            // FIXME: Is this cast valid?
            .get((idx - 1) as usize)
            .map(|s| s.to_owned())
            .unwrap_or_default(),
    };

    Some(JkString::from(result_string).to_instance())
}

fn arg_amount(ctx: &mut Context, _args: Args) -> Option<ObjectInstance> {
    Some(JkInt::from(ctx.args().len() as i64).to_instance())
}

/// Exit the interpreter with a given exit code
fn exit(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let exit_code = JkInt::from_instance(&args[0].execute(ctx).unwrap()).0;

    // FIXME: Is this cast valid?
    std::process::exit(exit_code as i32);
}

fn fmt_char(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let value = JkChar::from_instance(&args[0].execute(ctx).unwrap()).0;

    Some(JkString::from(value.to_string()).to_instance())
}

fn fmt_float(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let value = JkFloat::from_instance(&args[0].execute(ctx).unwrap()).0;

    Some(JkString::from(value.to_string()).to_instance())
}

fn size_of(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let instance = args[0].execute(ctx).unwrap();

    Some(JkInt::from(instance.size() as i64).to_instance())
}

fn type_of(ctx: &mut Context, args: Args) -> Option<ObjectInstance> {
    let instance = args[0].execute(ctx).unwrap();
    let instance_ty = instance.ty().to_string();

    Some(JkString::from(instance_ty).to_instance())
}

impl Builtins {
    fn add(&mut self, name: &'static str, builtin_fn: BuiltinFn) {
        self.functions.insert(String::from(name), builtin_fn);
    }

    /// Create a new instance of builtins, with pre-defined functions
    pub fn new() -> Builtins {
        let mut builtins = Builtins {
            functions: HashMap::new(),
        };

        builtins.add("__builtin_string_len", string_len);
        builtins.add("__builtin_string_concat", string_concat);
        builtins.add("__builtin_string_display", string_display);
        builtins.add("__builtin_string_display_err", string_display_err);
        builtins.add("__builtin_string_is_empty", string_is_empty);
        builtins.add("__builtin_string_equals", string_equals);
        builtins.add("__builtin_ffi_link_with", ffi_link_with);
        builtins.add("__builtin_fmt_char", fmt_char);
        builtins.add("__builtin_fmt_float", fmt_float);
        builtins.add("__builtin_arg_get", arg_get);
        builtins.add("__builtin_arg_amount", arg_amount);
        builtins.add("__builtin_exit", exit);
        builtins.add("size_of", size_of);
        builtins.add("type_of", type_of);

        builtins
    }

    pub fn contains(&self, name: &str) -> bool {
        // We can demangle builtins to dispatch to our single, non generic
        // implementation.
        // let name = generics::original_name(name);

        self.functions.contains_key(name)
    }

    pub fn get(&self, builtin: &str) -> Option<&BuiltinFn> {
        // self.functions.get(generics::original_name(builtin))
        self.functions.get(builtin)
    }
}

impl Default for Builtins {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use crate::jinko;

    #[test]
    fn t_string_builtins_are_valid() {
        jinko! {
            __builtin_string_len("jk");
            __builtin_string_concat("file", ".jk");
            __builtin_string_display("to display", true);
            __builtin_string_display_err("to display on err", true);
            __builtin_string_equals("jin", "ko");
            __builtin_string_is_empty("jinko");
        };
    }

    #[test]
    #[cfg(feature = "ffi")]
    fn t_ffi_builtins_are_valid() {
        jinko! {
            __builtin_ffi_link_with("tests/fixtures/clib/lib.so");
        };
    }

    #[test]
    #[cfg(not(feature = "ffi"))]
    fn t_ffi_builtins_are_valid_no_ffi() {
        use crate::jinko_fail;

        jinko_fail! {
            __builtin_ffi_link_with("tests/fixtures/clib/lib.so");
        };
    }

    #[test]
    fn t_args_builtins_are_valid() {
        jinko! {
            __builtin_arg_get(158);
            amount = __builtin_arg_amount();
        };
    }

    #[test]
    fn t_exit_builtin_is_valid() {
        use libc::{c_int, fork, waitpid, WEXITSTATUS};

        let pid = unsafe { fork() };
        if pid == 0 {
            jinko! {
                __builtin_exit(42);
            };

            panic!("We should have exited by now");
        }

        let mut status: c_int = 0;
        let status_ptr: *mut c_int = &mut status;
        unsafe {
            waitpid(pid, status_ptr, 0);
        }

        assert_eq!(WEXITSTATUS(status), 42);
    }

    #[test]
    fn t_fmt_builtins_are_valid() {
        jinko! {
            __builtin_fmt_char('a');
            __builtin_fmt_float(7.4);
        };
    }
}
