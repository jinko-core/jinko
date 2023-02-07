use jinko::context::Context;
use jinko::io_trait::JkStdReader;
use std::io::{self, Result, Write};

fn main() -> Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut ctx = Context::new(Box::new(JkStdReader));
    let mut input = String::new();

    let mut prompt = "> ";

    loop {
        ctx.clear_errors();
        print!("{prompt}");
        stdout.flush()?;

        input.clear();
        stdin.read_line(&mut input)?;

        if ctx.eval(&input).is_err() {
            prompt = "x ";
        } else {
            prompt = "> ";
        }
    }
}
