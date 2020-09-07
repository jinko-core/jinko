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

    Repl::launch_repl();
}
