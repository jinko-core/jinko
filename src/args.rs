//! The `Args` module helps giving command line option to jinko

use structopt::StructOpt;

use std::path::PathBuf;

#[derive(StructOpt)]
#[structopt(name = "jinko", about = "The jinko interpreter")]
pub struct Args {
    #[structopt(short, long)]
    version: bool,

    #[structopt(short, long)]
    pub interactive: bool,

    #[structopt(parse(from_os_str))]
    pub input: Option<PathBuf>,
}

impl Args {
    fn print_version() {
        println!("{}", env!("CARGO_PKG_VERSION"));

        std::process::exit(0);
    }

    /// Parses the command line arguments, executes stopping options (such as --help
    /// or --version) and returns the given arguments
    pub fn handle() -> Args {
        let args = Args::from_args();

        if args.version {
            Args::print_version()
        }

        args
    }
}
