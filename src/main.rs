#[warn(missing_docs)]
mod args;
mod error;
mod instance;
mod instruction;
mod interpreter;
mod parser;
mod repl;
mod utils;
mod value;

use args::Args;
use parser::Parser;
use repl::Repl;
use std::fs;

pub use error::{JkErrKind, JkError};
pub use instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
pub use instruction::{InstrKind, Instruction};
pub use interpreter::Interpreter;
pub use value::{JkBool, JkChar, JkConstant, JkFloat, JkInt, JkString, Value};

fn handle_exit_code(result: InstrKind) {
    match result {
        // A statement that completes succesfully returns 0
        InstrKind::Statement | InstrKind::Expression(None) => std::process::exit(0),

        // FIXME: Maybe return different stuff based on more types?

        // If it's an expression, return if you can (if it's an int)
        InstrKind::Expression(Some(i)) => match i.ty() {
            Some(ty) => match ty.as_ref() {
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
    let mut interpreter = Parser::parse(&input).unwrap();

    interpreter.set_path(Some(path.to_owned()));
    interpreter.set_debug(args.debug());

    // The entry point always has a block
    let ep = interpreter.entry_point.block().unwrap().clone();
    match ep.execute(&mut interpreter) {
        Ok(result) => handle_exit_code(result),
        Err(e) => e.exit(),
    }
}
