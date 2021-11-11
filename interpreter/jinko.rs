// FIXME: Add #![warn(missing_docs)]

mod args;
mod repl;

use jinko::{
    CheckedType, Context, Error, FromObjectInstance, JkBool, JkFloat, JkInt, ObjectInstance,
};

use args::Args;
use repl::Repl;
use std::{fs, path::Path};

// FIXME: Add documentation
pub type InteractResult = Result<(Option<ObjectInstance>, Context), Error>;

fn handle_exit_code(result: Option<ObjectInstance>) -> ! {
    use std::process::exit;

    match result {
        // A statement that completes succesfully returns 0
        None => exit(0),

        // FIXME: Maybe return different stuff based on more types?

        // If it's an expression, return if you can (if it's an int)
        Some(i) => match i.ty() {
            CheckedType::Resolved(ty) => match ty.id() {
                "int" => exit(JkInt::from_instance(&i).rust_value() as i32),
                "float" => exit(JkFloat::from_instance(&i).rust_value() as i32),
                "bool" => {
                    let b_value = JkBool::from_instance(&i).rust_value();
                    match b_value {
                        true => exit(0),
                        false => exit(1),
                    }
                }
                _ => exit(0),
            },
            CheckedType::Void => exit(0),
            CheckedType::Unknown => unreachable!("this shouldn't happen"),
        },
    }
}

fn handle_input(args: &Args, file: &Path) -> InteractResult {
    let input = fs::read_to_string(file)?;

    let mut ctx = Context::new();

    if !args.nostdlib() {
        ctx.init_stdlib()?;
    }

    jinko::parse(&mut ctx, &input)?;

    ctx.set_path(Some(file.to_owned()));
    ctx.set_args(args.project_args());
    ctx.set_debug(args.debug());

    ctx.emit_errors();
    ctx.clear_errors();

    match args.interactive() {
        true => Repl::new(args)?.with_context(ctx).launch(),
        false => {
            let res = ctx.execute()?;
            ctx.emit_errors();

            Ok((res, ctx))
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::handle();

    let result = args.input().map_or_else(
        || Repl::new(&args)?.launch(),
        |filename| handle_input(&args, filename),
    )?;

    handle_exit_code(result.0)
}
