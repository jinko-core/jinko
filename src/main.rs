#[warn(missing_docs)]
mod args;
mod context;
mod error;
mod instance;
mod instruction;
mod parser;
mod repl;
mod utils;
mod value;

use args::Args;
use parser::Parser;
use repl::Repl;
use std::{fs, path::Path};

pub use context::Context;
pub use error::{ErrKind, Error};
pub use instance::{FromObjectInstance, ObjectInstance, ToObjectInstance};
pub use instruction::{InstrKind, Instruction, Rename};
pub use value::{JkBool, JkChar, JkConstant, JkFloat, JkInt, JkString, Value};

// FIXME: Add documentation
pub type InteractResult = Result<(Option<ObjectInstance>, Context), Error>;

fn handle_exit_code(result: Option<ObjectInstance>) -> ! {
    use std::process::exit;

    match result {
        // A statement that completes succesfully returns 0
        None => exit(0),

        // FIXME: Maybe return different stuff based on more types?

        // If it's an expression, return if you can (if it's an int)
        Some(i) => match i.ty() {
            Some(ty) => match ty.name() {
                "int" => exit(JkInt::from_instance(&i).0 as i32),
                "float" => exit(JkFloat::from_instance(&i).0 as i32),
                "bool" => {
                    let b_value = JkBool::from_instance(&i).0;
                    match b_value {
                        true => exit(0),
                        false => exit(1),
                    }
                }
                _ => exit(0),
            },
            None => exit(0),
        },
    }
}

fn handle_input(args: &Args, file: &Path) -> InteractResult {
    let input = fs::read_to_string(file)?;

    let mut ctx = Parser::parse(&input)?;
    ctx.set_path(Some(file.to_owned()));
    ctx.set_debug(args.debug());

    ctx.emit_errors();
    ctx.clear_errors();

    match args.interactive() {
        true => Repl::launch_with_context(ctx),
        false => {
            let res = ctx.execute()?;
            ctx.emit_errors();

            Ok((res, ctx))
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::handle();

    let result = args.input().map_or_else(
        || Repl::launch_repl(&args),
        |filename| handle_input(&args, filename),
    )?;

    handle_exit_code(result.0)
}
