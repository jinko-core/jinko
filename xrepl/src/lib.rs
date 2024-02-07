use builtins::AppendAstBuiltins;
use colored::Colorize;
use error::Error;
use flatten::FlattenAst;
use location::{Source, SpanTuple};

use fire::{instance::Instance, Interpret};
use name_resolve::NameResolve;
use rustyline::error::ReadlineError;
use typecheck::TypeCheck;

use std::{ops::ControlFlow, process};

// The "pipeline" loop. We basically want to get a context, add to it, typecheck the entire thing and keep everything nice and tight
fn pipeline(ast: &ast::Ast) -> ControlFlow<Error, ()> {
    // FIXME: do we want to accumulate errors instead here

    macro_rules! x_try {
        ($res:expr) => {
            match $res {
                Ok(inner) => inner,
                Err(e) => return ControlFlow::Break(e),
            }
        };
    }

    let fir = ast.flatten();
    let fir = x_try!(fir.name_resolve());
    let fir = x_try!(fir.type_check());

    let result = fir.interpret();

    if let Some(instance) = &result {
        match instance {
            Instance::Int(inner) => println!("{inner}"),
            Instance::Float(inner) => println!("{inner}"),
            Instance::Bool(inner) => println!("{inner}"),
            Instance::Char(inner) => println!("{inner}"),
            Instance::String(inner) => println!("{inner}"),
            Instance::Record { ty, .. } => println!("// #{ty}"),
            Instance::SlowRecord(map) => println!("// {map:?}"),
            Instance::Empty => {}
        }
    }

    ControlFlow::Continue(())
}

// TODO: Use proper types here
pub fn repl() -> Result<(), Error> {
    let mut rl = rustyline::DefaultEditor::new().unwrap();
    // FIXME: How to handle this?
    let builtin_types = "type unit; type bool; type int; type float; type char; type string;";
    let ast = xparser::parse(builtin_types, Source::Input(builtin_types)).unwrap();

    // FIXME: No unwrap
    let mut ast = ast.append_builtins().unwrap();

    loop {
        let line = rl.readline(&format!("{} > ", "jinko".purple()));

        // let mut fir = ast.flatten();

        match line {
            Ok(line) => {
                rl.add_history_entry(&line).unwrap();

                // this returns a block, from which we extract the only expression and set ast's `last_is_expr`
                let expr = xparser::parse(&line, location::Source::Input(&line));
                let expr = match expr {
                    Ok(expr) => expr,
                    Err(e) => {
                        dbg!(e);
                        continue;
                    }
                };

                let expr = match expr.node {
                    ast::Node::Block { mut stmts, .. } => stmts.pop().unwrap(),
                    _ => unreachable!(),
                };

                // FIXME: NO unwrap above

                let mut stmts = match &ast.node {
                    ast::Node::Block { stmts, .. } => stmts.clone(),
                    _ => unreachable!(),
                };

                stmts.push(expr);

                let tmp_ast = ast::Ast {
                    location: SpanTuple::builtin(),
                    node: ast::Node::Block {
                        stmts,
                        last_is_expr: true,
                    },
                };

                match pipeline(&tmp_ast) {
                    ControlFlow::Continue(_) => {
                        ast = tmp_ast;
                    }
                    ControlFlow::Break(e) => {
                        e.emit();
                        continue;
                    }
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => {
                println!("see you!");
                process::exit(0)
            }
            Err(e) => {
                eprintln!("oops: {e}")
            }
        }
    }
}
