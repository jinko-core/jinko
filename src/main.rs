mod args;
mod block;
mod interpreter;
mod instruction;
mod parser;
mod value;

use args::Args;

fn main() {
    let args = Args::handle();

    println!("{:#?}", args.input);
}
