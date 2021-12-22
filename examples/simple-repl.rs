use jinko::Context;
use std::io::{self, BufRead, Result, Write};

fn main() -> Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();

    let mut ctx = Context::new();
    let mut input = String::new();

    loop {
        print!("> ");
        stdout.flush()?;

        input.clear();
        stdin.read_line(&mut input)?;

        // ctx.eval(input);

        print!("{}", input);
    }
}
