//! This module takes care of performing calls to extern functions. Extern functions will
//! either be builtins, or C functions defined in a dynamic library loaded with the `link_with`
//! builtin.

use fir::{Node, RefIdx};
use flatten::FlattenData;

use crate::instance::Instance;
use crate::GarbaJKollector;

// TODO: Factor this with `ast_builtins`?
static ARITHMETIC_BUILTINS: &[&'static str] = &["+", "-", "*", "/"];

enum Arithmetic {
    Add,
    Sub,
    Mul,
    Div,
}

impl From<&str> for Arithmetic {
    fn from(s: &str) -> Self {
        match s {
            "+" => Arithmetic::Add,
            "-" => Arithmetic::Sub,
            "*" => Arithmetic::Mul,
            "/" => Arithmetic::Div,
            _ => unreachable!("this is an interpreter error."),
        }
    }
}

pub fn perform_call(
    // FIXME: This should probably take a context, correct? &self?
    gc: &GarbaJKollector,
    node: &Node<FlattenData<'_>>,
    args: &[RefIdx],
) -> Option<Instance> {
    let ast = node.data.ast.node();
    let name = match &ast.node {
        ast::Node::Function {
            decl: ast::Declaration { name, .. },
            ..
        } => name,
        other => {
            dbg!(other);
            unreachable!()
        }
    };

    if ARITHMETIC_BUILTINS.contains(&name.access()) {
        return Some(arithmetic_builtin_call(
            gc,
            args,
            Arithmetic::from(name.access()),
        ));
    }

    if name.access() == "println" {
        args.iter().for_each(|arg| {
            let value = gc.lookup(&arg.expect_resolved()).unwrap();
            if let Instance::String(s) = value {
                println!("{s}");
            } else {
                unreachable!("typecheck didn't catch this error. this is an interpreter bug.");
            }
        })
    }

    None
}

fn int_op(lhs: &i64, rhs: &i64, op: Arithmetic) -> Instance {
    let res = match op {
        Arithmetic::Add => lhs + rhs,
        Arithmetic::Sub => lhs - rhs,
        Arithmetic::Mul => lhs * rhs,
        Arithmetic::Div => lhs / rhs,
    };

    Instance::Int(res)
}

fn float_op(lhs: &f64, rhs: &f64, op: Arithmetic) -> Instance {
    let res = match op {
        Arithmetic::Add => lhs + rhs,
        Arithmetic::Sub => lhs - rhs,
        Arithmetic::Mul => lhs * rhs,
        Arithmetic::Div => lhs / rhs,
    };

    Instance::Float(res)
}

fn arithmetic_builtin_call(gc: &GarbaJKollector, args: &[RefIdx], op: Arithmetic) -> Instance {
    let lhs = gc.lookup(&args[0].expect_resolved()).unwrap();
    let rhs = gc.lookup(&args[1].expect_resolved()).unwrap();

    match (lhs, rhs) {
        (Instance::Int(l), Instance::Int(r)) => int_op(l, r, op),
        (Instance::Float(l), Instance::Float(r)) => float_op(l, r, op),
        _ => unreachable!("fuck me"),
    }
}
