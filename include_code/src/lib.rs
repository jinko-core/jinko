use ast::{Ast, Visitor};

pub trait IncludeCode {
    fn resolve_includes(self) -> Self;
}

struct IncludeCtx;

impl Visitor for IncludeCtx {}

impl IncludeCode for Ast {
    fn resolve_includes(self) -> Ast {
        let mut ctx = IncludeCtx;

        // Basically what we want to do is visit each node. When we see a `Node::Incl`, we simply fetch its content and return a new node.
        // At this point, we assume the AST is valid. Right? No! We can return an Err::Incl if something goes south.
        ctx.visit(self)
    }
}

#[cfg(test)]
mod tests {}
