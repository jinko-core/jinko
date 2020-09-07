mod args;
mod error;
mod instruction;
mod interpreter;
mod parser;
mod repl;
mod value;

use args::Args;

fn main() {
    let args = Args::handle();

    println!("{:#?}", args.input);
}
