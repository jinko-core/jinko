// FIXME: Add #![warn(missing_docs)]

mod args;
#[cfg(feature = "repl")]
mod repl;

use ast::Ast;
use colored::Colorize;

use builtins::AppendAstBuiltins;
use dedup::DeduplicateConstants;
use fire::instance::Instance;
use fire::Interpret;
use flatten::{FlattenAst, FlattenData};
use include_code::IncludeCode;
use loop_desugar::DesugarLoops;
use name_resolve::NameResolve;

use jinko::context::Context;
use jinko::error::{ErrKind, Error};
use jinko::instance::{FromObjectInstance, ObjectInstance};
use jinko::location::Source;
use jinko::typechecker::CheckedType;
use jinko::value::{JkBool, JkFloat, JkInt};

use args::Args;
#[cfg(feature = "repl")]
use repl::Repl;
use std::process;
use std::{fs, path::Path};
use symbol::Symbol;
use typecheck::TypeCheck;

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
            CheckedType::Error | CheckedType::Later => unreachable!("this shouldn't happen"),
        },
    }
}

fn run_tests(ctx: &mut Context) -> Result<Option<ObjectInstance>, Error> {
    let res = ctx.execute()?;
    let tests: Vec<String> = ctx
        .tests()
        .iter()
        .map(|(k, _)| k.into())
        .filter(|test_name| {
            if !ctx.args().is_empty() {
                ctx.args().contains(test_name)
            } else {
                true
            }
        })
        .collect();

    for test_name in tests {
        eprint!(
            "[{}] running test `{}`... ",
            "WTNG".yellow().blink(),
            test_name
        );
        let fdec = ctx.tests().get(&test_name).unwrap().clone();
        let _test_result = fdec.run(ctx);
        eprintln!("\r[ {} ] running test `{}`... ", "OK".green(), test_name);

        // FIXME: We should think about handling error values in tests
        // match test_result.unwrap().ty() {
        //     CheckedType::Resolved(ty) => match ty.id() {
        //         "Ok" => eprintln!("{}", "OK".green()),
        //         "Err" => eprintln!("{}", "KO".red()),
        //         _ => unreachable!(), // FIXME:
        //     },
        //     _ => unreachable!(), // FIXME:
        //     }
    }

    ctx.emit_errors();

    Ok(res)
}

fn experimental_pipeline(input: &str, file: &Path) -> InteractResult {
    use debug_fir::FirDebug;

    // FIXME: do we want to accumulate errors instead here

    macro_rules! x_try {
        ($res:expr) => {
            match $res {
                Ok(inner) => inner,
                Err(e) => {
                    e.emit();
                    return Err(Error::new(ErrKind::Context));
                }
            }
        };
    }

    let ast = xparser::parse(input, Source::Path(file))?;

    let ast = x_try!(ast.desugar_loops());
    let ast = x_try!(ast_sanitizer::only_while_loops(ast));

    let ast = x_try!(ast.resolve_includes());
    let ast = x_try!(ast_sanitizer::no_incl(ast));

    // TODO: Add a builtins sanitizer pass?
    let ast = x_try!(ast.append_builtins());

    let data_fmt = |data: &FlattenData| {
        format!(
            "{} @ scope {}",
            data.ast
                .symbol()
                .as_ref()
                .map(|s| format!("`{}`", Symbol::access(s)))
                .or(match data.ast {
                    flatten::AstInfo::Node(Ast {
                        node: ast::Node::Constant(value),
                        ..
                    }) => Some(value.to_string()),
                    _ => None,
                })
                .unwrap_or_default(),
            data.scope
        )
    };

    let fir = ast.flatten();
    FirDebug::default()
        .header("flattened")
        .show_data(data_fmt)
        .display(&fir);

    let fir = x_try!(fir.name_resolve());
    FirDebug::default()
        .header("name_resolved")
        .show_data(data_fmt)
        .display(&fir);

    let fir = x_try!(fir.deduplicate_constants());
    FirDebug::default()
        .header("deduped_constants")
        .show_data(data_fmt)
        .display(&fir);

    let fir = x_try!(fir.type_check());
    FirDebug::default()
        .header("typechecked")
        .show_data(data_fmt)
        .display(&fir);

    let result = fir.interpret();

    let exit_code = match result {
        // convert `true` to `0` and `false` to `1`
        Some(Instance::Bool(b)) => !b as i32,
        Some(Instance::Int(inner)) => inner as i32,
        _ => 0,
    };

    process::exit(exit_code);
}

fn handle_input(args: &Args, file: &Path) -> InteractResult {
    let input = fs::read_to_string(file)?;

    if args.experimental {
        experimental_pipeline(&input, file)
    } else {
        let mut ctx = Context::new(Box::new(jinko::io_trait::JkStdReader));

        if !args.nostdlib() {
            ctx.init_stdlib()?;
        }

        jinko::parser::parse(&mut ctx, &input, Source::Path(file))?;

        ctx.set_path(Some(file.to_owned()));
        ctx.set_args(args.project_args());

        ctx.emit_errors();
        ctx.clear_errors();

        if args.check() {
            ctx.check()?;
            ctx.emit_errors();

            return Ok((None, ctx));
        }

        match args.test() {
            false => match args.interactive() {
                #[cfg(feature = "repl")]
                true => Repl::new()?.with_context(ctx).launch(),
                #[cfg(not(feature = "repl"))]
                true => panic!("Jinko is not compiled with repl support"),
                false => {
                    let res = ctx.execute()?;
                    ctx.emit_errors();

                    Ok((res, ctx))
                }
            },
            true => {
                if args.interactive() {
                    return Err(Error::new(ErrKind::Context)
                        .with_msg(String::from("cannot run tests in interactive mode")));
                }

                let res = run_tests(&mut ctx)?;

                Ok((res, ctx))
            }
        }
    }
}

fn main() -> anyhow::Result<()> {
    let args = Args::handle();
    if args.debug() {
        jinko::debug::enable();
    }
    #[cfg(feature = "repl")]
    let result = args.input().map_or_else(
        || {
            if args.experimental {
                match xrepl::repl() {
                    // FIXME: At the moment, we are still expected to return an "old" [`Context`] in this block - even
                    // if it does not make sense with the new interpreter and intermediate representation. So build a
                    // fake and empty context for now, but remove it in the future
                    Ok(_) => Ok((None, Context::new(Box::new(jinko::io_trait::JkStdReader)))),
                    Err(_) => todo!(),
                }
            } else {
                Repl::new()?.launch()
            }
        },
        |filename| handle_input(&args, filename),
    )?;

    #[cfg(not(feature = "repl"))]
    let result = args
        .input()
        .map(|filename| handle_input(&args, filename))
        .unwrap()?;

    handle_exit_code(result.0)
}
