//! The `Args` module helps giving command line option to jinko

use structopt::StructOpt;

use std::path::PathBuf;

#[derive(StructOpt)]
#[structopt(name = "jinko", about = "The jinko ctx")]
pub struct Args {
    #[structopt(short, long)]
    version: bool,

    #[structopt(short, long)]
    interactive: bool,

    #[structopt(long = "no-std")]
    nostdlib: bool,

    #[structopt(short, long)]
    debug: bool,

    #[structopt(parse(from_os_str))]
    input: Option<PathBuf>,

    #[structopt()]
    arguments: Vec<String>,
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

    /// Is the context launched in interactive mode
    pub fn interactive(&self) -> bool {
        self.interactive
    }

    /// Is the context launched in debug mode
    pub fn debug(&self) -> bool {
        self.debug
    }

    /// Is the context launched without stdlib
    pub fn nostdlib(&self) -> bool {
        self.nostdlib
    }

    /// Arguments given to the program
    pub fn project_args(&self) -> Vec<String> {
        self.arguments.clone()
    }

    /// File input given to the context
    pub fn input(&self) -> Option<&PathBuf> {
        self.input.as_ref()
    }
}
