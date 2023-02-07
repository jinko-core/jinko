use ast::{Ast, LoopKind, Node, Visitor};
use error::{ErrKind, Error};
use location::SpanTuple;
use symbol::Symbol;

struct NoInclCtx;

impl Visitor for NoInclCtx {
    fn visit_incl(&mut self, loc: SpanTuple, _: Symbol, _: Option<Symbol>) -> Result<Ast, Error> {
        Err(Error::new(ErrKind::Sanitizer)
            .with_loc(Some(loc))
            .with_msg("include expression detected when none should exist".to_string()))
    }
}

struct OnlyWhileCtx;

impl Visitor for OnlyWhileCtx {
    fn visit_loop(
        &mut self,
        location: SpanTuple,
        kind: LoopKind,
        block: Box<Ast>,
    ) -> Result<Ast, Error> {
        match kind {
            LoopKind::Infinite | LoopKind::For { .. } => Err(Error::new(ErrKind::Sanitizer)
                .with_loc(Some(location))
                .with_msg("`for` loop or `loop` loop did not get desugared correctly".to_string())),
            _ => Ok(Ast {
                location,
                node: Node::Loop(kind, block),
            }),
        }
    }
}

pub fn no_incl(ast: Ast) -> Result<Ast, Error> {
    let mut ctx = NoInclCtx;

    ctx.visit(ast)
}

pub fn only_while_loops(ast: Ast) -> Result<Ast, Error> {
    let mut ctx = OnlyWhileCtx;

    ctx.visit(ast)
}
