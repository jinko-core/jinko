mod args;
mod error;
mod instruction;
mod interpreter;
mod parser;
mod repl;
mod value;

use args::Args;
use repl::Repl;

fn main() {
    let args = Args::handle();

    match Repl::launch_repl() {
        Ok(_) => {}
        Err(e) => e.exit(),
    }
}
