//! This module takes care of performing calls to extern functions. Extern functions will
//! either be builtins, or C functions defined in a dynamic library loaded with the `link_with`
//! builtin.

use fir::{Node, RefIdx};
use flatten::FlattenData;

use builtins::Operator;

use builtins::Arithmetic::*;
use builtins::Comparison::*;
use builtins::Mode::*;
use builtins::Unary::*;

use crate::instance::Instance;
use crate::GarbaJKollector;

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

    if let Some(op) = builtins::Operator::try_from_str(name.access()) {
        return Some(arithmetic_builtin_call(gc, args, op));
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

fn bool_ops(lhs: &bool, rhs: &bool, op: builtins::Operator) -> Instance {
    match op {
        Operator::Unary(Not) => Instance::from(!lhs),
        Operator::Comparison(Equals) => Instance::from(lhs == rhs),
        Operator::Comparison(Differs) => Instance::from(lhs != rhs),
        _ => unreachable!(
            "invalid operation on booleans: `{}`. this is an intepreter error",
            op.as_str()
        ),
    }
}

fn int_ops(lhs: &i64, rhs: &i64, op: builtins::Operator) -> Instance {
    match op {
        // operations returning integers
        Operator::Arithmetic(Add) => Instance::from(lhs + rhs),
        Operator::Arithmetic(Sub) => Instance::from(lhs - rhs),
        Operator::Arithmetic(Mul) => Instance::from(lhs * rhs),
        Operator::Arithmetic(Div) => Instance::from(lhs / rhs),
        Operator::Unary(Minus) => Instance::from(-lhs),
        // operations returning booleans
        Operator::Comparison(Equals) => Instance::from(lhs == rhs),
        Operator::Comparison(Differs) => Instance::from(lhs != rhs),
        Operator::Comparison(LessThan(Strict)) => Instance::from(lhs < rhs),
        Operator::Comparison(LessThan(OrEqual)) => Instance::from(lhs <= rhs),
        Operator::Comparison(GreaterThan(Strict)) => Instance::from(lhs > rhs),
        Operator::Comparison(GreaterThan(OrEqual)) => Instance::from(lhs >= rhs),
        _ => unreachable!(
            "invalid operation on integers: `{}`. this is an intepreter error",
            op.as_str()
        ),
    }
}

fn char_ops(lhs: &char, rhs: &char, op: builtins::Operator) -> Instance {
    match op {
        Operator::Comparison(Equals) => Instance::from(lhs == rhs),
        Operator::Comparison(Differs) => Instance::from(lhs != rhs),
        Operator::Comparison(LessThan(Strict)) => Instance::from(lhs < rhs),
        Operator::Comparison(LessThan(OrEqual)) => Instance::from(lhs <= rhs),
        Operator::Comparison(GreaterThan(Strict)) => Instance::from(lhs > rhs),
        Operator::Comparison(GreaterThan(OrEqual)) => Instance::from(lhs >= rhs),
        _ => unreachable!(
            "invalid operation on chars: `{}`. this is an intepreter error",
            op.as_str()
        ),
    }
}

fn float_ops(lhs: &f64, rhs: &f64, op: builtins::Operator) -> Instance {
    match op {
        // operations returning integers
        Operator::Arithmetic(Add) => Instance::from(lhs + rhs),
        Operator::Arithmetic(Sub) => Instance::from(lhs - rhs),
        Operator::Arithmetic(Mul) => Instance::from(lhs * rhs),
        Operator::Arithmetic(Div) => Instance::from(lhs / rhs),
        Operator::Unary(Minus) => Instance::from(-lhs),
        // operationrs returning booleans
        Operator::Comparison(Equals) => Instance::from(lhs == rhs),
        Operator::Comparison(Differs) => Instance::from(lhs != rhs),
        Operator::Comparison(LessThan(Strict)) => Instance::from(lhs < rhs),
        Operator::Comparison(LessThan(OrEqual)) => Instance::from(lhs <= rhs),
        Operator::Comparison(GreaterThan(Strict)) => Instance::from(lhs > rhs),
        Operator::Comparison(GreaterThan(OrEqual)) => Instance::from(lhs >= rhs),
        _ => unreachable!(
            "invalid operation on floats: `{}`. this is an intepreter error",
            op.as_str()
        ),
    }
}

fn string_ops(lhs: &str, rhs: &str, op: builtins::Operator) -> Instance {
    match op {
        Operator::Comparison(Equals) => Instance::from(lhs == rhs),
        Operator::Comparison(Differs) => Instance::from(lhs != rhs),
        _ => unreachable!(
            "invalid operation on strings: `{}`. this is an intepreter error",
            op.as_str()
        ),
    }
}

fn arithmetic_builtin_call(gc: &GarbaJKollector, args: &[RefIdx], op: Operator) -> Instance {
    let lhs = gc.lookup(&args[0].expect_resolved()).unwrap();
    let rhs = gc.lookup(&args[1].expect_resolved()).unwrap();

    match (lhs, rhs) {
        (Instance::Bool(l), Instance::Bool(r)) => bool_ops(l, r, op),
        (Instance::Int(l), Instance::Int(r)) => int_ops(l, r, op),
        (Instance::Char(l), Instance::Char(r)) => char_ops(l, r, op),
        (Instance::Float(l), Instance::Float(r)) => float_ops(l, r, op),
        (Instance::String(l), Instance::String(r)) => string_ops(l, r, op),
        _ => unreachable!("invalid type for arithmetic operation. this is an interpreter error."),
    }
}
