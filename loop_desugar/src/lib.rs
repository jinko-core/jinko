//! This pass transforms all instances of `for` loops and infinite loops into
//! `while` loops.
//! The process is as follows:
//!
//! ## `for` loops
//!
//! original form:
//!
//! ```text
//! for <iter> in <range> <block>
//! ```
//!
//! after desugar:
//!
//! ```text
//! {
//!     __#range = <range>;
//!     while __#range.has_next() {
//!         <iter> = __#range.next().unpack();
//!         <block>
//!     }
//! }
//! ```
//!
//! ## `loop` loops
//!
//! original form:
//!
//! ```text
//! loop <block>
//! ```
//!
//! after desugar:
//!
//! ```text
//! while true <block>
//! ```

use ast::{Ast, Call, LoopKind, Node, Value, Visitor};
use error::Error;
use location::SpanTuple;
use symbol::Symbol;

pub trait DesugarLoops: Sized {
    fn desugar_loops(self) -> Result<Self, Error>;
}

impl DesugarLoops for Ast {
    fn desugar_loops(self) -> Result<Ast, Error> {
        let mut ctx = Ctx;

        ctx.visit(self)
    }
}

struct Ctx;

fn handle_loop_loop(loc: SpanTuple, block: Box<Ast>) -> Ast {
    let kind = LoopKind::While(Box::new(Ast {
        location: loc.clone(),
        node: Node::Constant(Value::Bool(true)),
    }));

    Ast {
        location: loc,
        node: Node::Loop(kind, block),
    }
}

fn call(to: &str, args: Vec<Ast>) -> Call {
    Call {
        to: Symbol::from(to),
        generics: vec![],
        args,
    }
}

fn method(loc: &SpanTuple, instance: Box<Ast>, call: Call) -> Ast {
    Ast {
        location: loc.clone(),
        node: Node::MethodCall { instance, call },
    }
}

fn var_assign<S: Into<Symbol>>(loc: &SpanTuple, to_assign: S, value: Box<Ast>) -> Ast {
    Ast {
        location: loc.clone(),
        node: Node::VarAssign {
            mutable: false,
            to_assign: to_assign.into(),
            value,
        },
    }
}

fn block(loc: &SpanTuple, stmts: Vec<Ast>, last_is_expr: bool) -> Ast {
    Ast {
        location: loc.clone(),
        node: Node::Block {
            stmts,
            last_is_expr,
        },
    }
}

fn var<S: Into<Symbol>>(loc: &SpanTuple, name: S) -> Ast {
    Ast {
        location: loc.clone(),
        node: Node::Var(name.into()),
    }
}

// TODO: Add macros to help generate AST fragments here
fn handle_for_loop(loc: SpanTuple, iterator: Symbol, range: Box<Ast>, loop_block: Box<Ast>) -> Ast {
    let range_var = "__#range";
    let (stmts, last_is_expr) = match loop_block.node {
        Node::Block {
            stmts,
            last_is_expr,
        } => (stmts, last_is_expr),
        _ => unreachable!(
            "invalid AST: non-block expression in loop: {:?}",
            loop_block
        ),
    };

    fn inner(
        loc: &SpanTuple,
        iterator: Symbol,
        range: Box<Ast>,
        stmts: Vec<Ast>,
        last_is_expr: bool,
    ) -> Ast {
        // while <range>.has_next()
        let range_has_next = method(loc, range.clone(), call("has_next", vec![]));
        let kind = LoopKind::While(Box::new(range_has_next));

        // FIXME: Missing call to `unpack`
        // __#range.next()
        let iter_next = method(loc, range, call("next", vec![]));

        // <iterator> = __#range.next();
        let assignment = var_assign(loc, iterator, Box::new(iter_next));

        // {
        //     <iterator> = __#range.next();
        //     <stmts>
        // }
        let mut new_stmts = vec![assignment];
        stmts.into_iter().for_each(|stmt| new_stmts.push(stmt));
        let block = block(loc, new_stmts, last_is_expr);

        Ast {
            location: loc.clone(),
            node: Node::Loop(kind, Box::new(block)),
        }
    }

    // __#range = <range>;
    let range_create = var_assign(&loc, range_var, range);

    // __#range
    let new_range = var(&loc, range_var);

    let new_while = inner(&loc, iterator, Box::new(new_range), stmts, last_is_expr);

    // {
    //     __#range = <range>;
    //     <new_while>
    // }
    let stmts = vec![range_create, new_while];

    Ast {
        location: loc,
        node: Node::Block {
            stmts,
            last_is_expr,
        },
    }
}

impl Visitor for Ctx {
    fn visit_loop(
        &mut self,
        location: SpanTuple,
        kind: LoopKind,
        block: Box<Ast>,
    ) -> Result<Ast, Error> {
        let node = match kind {
            LoopKind::Infinite => handle_loop_loop(location, block),
            LoopKind::For { iterator, range } => handle_for_loop(location, iterator, range, block),
            // `while` loops do not get desugared
            w => Ast {
                location,
                node: Node::Loop(w, block),
            },
        };

        Ok(node)
    }
}

// FIXME: Add tests
