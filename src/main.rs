mod args;
mod error;
mod instruction;
mod interpreter;
mod parser;
mod repl;
mod value;

use std::fs;
use args::Args;
use repl::Repl;
use parser::Parser;

fn main() {
    let args = Args::handle();

    if args.interactive || args.input.is_none() {
        match Repl::launch_repl() {
            Ok(_) => {}
            Err(e) => e.exit(),
        }
    };

    // We can unwrap since we checked for `None` in the if
    let input = fs::read_to_string(args.input.unwrap()).unwrap();

    // FIXME: No unwrap()
    let mut interpreter = Parser::parse(&input).unwrap();

    interpreter.run_once();
}
