//! This pass transforms all instances of `for` loops and infinite loops into
//! `while` loops.
//! The process is as follows:
//!
//! ## `for` loops
//!
//! original form:
//!
//! ```
//! for <iter> in <range> <block>
//! ```
//!
//! after desugar:
//!
//! ```
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
//! ```
//! loop <block>
//! ```
//!
//! after desugar:
//!
//! ```
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

// TODO: Add macros to help generate AST fragments here
fn handle_for_loop(loc: SpanTuple, iterator: Symbol, range: Box<Ast>, block: Box<Ast>) -> Ast {
    let range_var = "__#range";
    let (stmts, last_is_expr) = match block.node {
        Node::Block {
            stmts,
            last_is_expr,
        } => (stmts, last_is_expr),
        _ => unreachable!("invalid AST: non-block expression in loop: {:?}", block),
    };

    fn inner(
        loc: SpanTuple,
        iterator: Symbol,
        range: Box<Ast>,
        stmts: Vec<Ast>,
        last_is_expr: bool,
    ) -> Ast {
        // while <range>.has_next()
        let range_has_next = Ast {
            location: loc.clone(),
            node: Node::MethodCall {
                instance: range.clone(),
                call: Call {
                    to: Symbol::from("has_next"),
                    ..Call::default()
                },
            },
        };
        let kind = LoopKind::While(Box::new(range_has_next));

        // FIXME: Missing call to `unpack`
        // __#range.next()
        let iter_next = Ast {
            location: loc.clone(),
            node: Node::MethodCall {
                instance: range,
                call: Call {
                    to: Symbol::from("next"),
                    ..Call::default()
                },
            },
        };

        // <iterator> = __#range.next();
        let assignment = Ast {
            location: loc.clone(),
            node: Node::VarAssign {
                mutable: false,
                to_assign: iterator,
                value: Box::new(iter_next),
            },
        };

        // {
        //     <iterator> = __#range.next();
        //     <stmts>
        // }
        let mut new_stmts = vec![assignment];
        stmts.into_iter().for_each(|stmt| new_stmts.push(stmt));
        let block = Ast {
            location: loc.clone(),
            node: Node::Block {
                stmts: new_stmts,
                last_is_expr,
            },
        };

        Ast {
            location: loc,
            node: Node::Loop(kind, Box::new(block)),
        }
    }

    // __#range = <range>;
    let range_create = Ast {
        location: loc.clone(),
        node: Node::VarAssign {
            mutable: false,
            to_assign: Symbol::from(range_var),
            value: range,
        },
    };

    // __#range
    let new_range = Ast {
        location: loc.clone(),
        node: Node::Var(Symbol::from(range_var)),
    };

    let new_while = inner(
        loc.clone(),
        iterator,
        Box::new(new_range),
        stmts,
        last_is_expr,
    );

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

impl Visitor<Error> for Ctx {
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
