#[warn(missing_docs)]
mod args;
mod error;
mod instruction;
mod interpreter;
mod parser;
mod repl;
mod value;

use args::Args;
use parser::Parser;
use repl::Repl;
use std::fs;

use instruction::Instruction;

pub use error::JinkoError;
pub use interpreter::Interpreter;

fn main() {
    let args = Args::handle();

    if args.interactive || args.input.is_none() {
        match Repl::launch_repl(&args) {
            Ok(_) => {}
            Err(e) => e.exit(),
        }
    };

    // We can unwrap since we checked for `None` in the if
    let input = fs::read_to_string(args.input.unwrap()).unwrap();

    // FIXME: No unwrap()
    let mut interpreter = Parser::parse(&input).unwrap();

    interpreter.debug_mode = args.debug;

    // The entry point always has a block
    let ep = interpreter.entry_point.block().unwrap().clone();
    match ep.execute(&mut interpreter) {
        Ok(_) => {}
        Err(e) => e.exit(),
    }
}
