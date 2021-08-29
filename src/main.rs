#[warn(missing_docs)]
mod args;
mod error;
mod instance;
mod instruction;
mod context;
mod parser;
mod repl;
mod utils;
mod value;

use args::Args;
use parser::Parser;
use repl::Repl;
use std::fs;

pub use error::{ErrKind, Error};
pub use instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
pub use instruction::{InstrKind, Instruction, Rename};
pub use context::Context;
pub use value::{JkBool, JkChar, JkConstant, JkFloat, JkInt, JkString, Value};

fn handle_exit_code(result: Option<ObjectInstance>) {
    match result {
        // A statement that completes succesfully returns 0
        None => std::process::exit(0),

        // FIXME: Maybe return different stuff based on more types?

        // If it's an expression, return if you can (if it's an int)
        Some(i) => match i.ty() {
            Some(ty) => match ty.name() {
                "int" => std::process::exit(JkInt::from_instance(&i).0 as i32),
                "float" => std::process::exit(JkFloat::from_instance(&i).0 as i32),
                "bool" => {
                    let b_value = JkBool::from_instance(&i).0;
                    match b_value {
                        true => std::process::exit(0),
                        false => std::process::exit(1),
                    }
                }
                _ => std::process::exit(0),
            },
            None => std::process::exit(0),
        },
    }
}

fn main() {
    let args = Args::handle();

    if args.interactive() || args.input().is_none() {
        match Repl::launch_repl(&args) {
            Ok(_) => return,
            Err(e) => e.exit(),
        }
    };

    // We can unwrap since we checked for `None` in the if
    let path = args.input().unwrap();

    let input = fs::read_to_string(&path).unwrap();

    // FIXME: No unwrap()
    let mut ctx = Parser::parse(&input).unwrap();
    ctx.emit_errors();
    ctx.clear_errors();

    ctx.set_path(Some(path.to_owned()));
    ctx.set_debug(args.debug());

    match ctx.execute() {
        Ok(result) => handle_exit_code(result),
        Err(e) => e.exit(),
    }
}
