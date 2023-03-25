//! [`Fire`] stands for [`Fir`] + Evaluator/Engine/Executor. It is an interpretor of jinko's
//! [`Fir`] internal representation. Some of the choices made in this executor are extremely
//! jinko-specific, and will not map well to other languages.

use ast::Node as AstNode;
use error::{ErrKind, Error};
use fir::{Fir, Incomplete, Kind, Mapper, Node, OriginIdx, RefIdx};
use flatten::{AstInfo, FlattenData};

// TODO: How to keep values here? This is where we do garbage collection - how to do it in a smart way?
struct FireCtx {}

struct Value(Option<ast::Value>);

impl From<FlattenData<'_>> for Value {
    fn from(data: FlattenData<'_>) -> Self {
        // FIXME: This does not seem very correct - how does it work for example for binops?
        match data.ast {
            AstInfo::Node(ast) => match &ast.node {
                AstNode::Constant(value) => Value(Some(value.clone())),
                _ => Value(None),
            },
            _ => Value(None),
        }
    }
}

// TODO: Should this be a multi mapper so it can map from one node to zero nodes?
impl Mapper<FlattenData<'_>, Value, Error> for FireCtx {
    fn map_constant(
        &mut self,
        data: FlattenData<'_>,
        origin: OriginIdx,
        constant: RefIdx,
    ) -> Result<Node<Value>, Error> {
        let ast = data.ast.node();

        let value = match &ast.node {
            AstNode::Constant(value) => value.clone(),
            _ => unreachable!(),
        };

        // We need our context to keep track of the values
        // we can probably reuse some stuff from our old ObjectInstance implementation

        todo!()
    }
}

pub trait Fire: Sized {
    // TODO: Evaluate should probably consume the Fir, and return the result of the last node? Does that make sense or not in an Fir context?
    /// Evaluate an instance of [`Fir`] within the Fir Engine
    fn evaluate(self) -> Result<Option<ast::Value>, Error>;
}

impl Fire for Fir<FlattenData<'_>> {
    // FIXME: Should this map to a Result<Option<Value>, Error> instead?
    // FIXME: What's a value here? an AST one?
    fn evaluate(self) -> Result<Option<ast::Value>, Error> {
        let mut ctx = FireCtx {};

        match ctx.map(self) {
            // how do we get the last value here? Since map returns a new Fir? We return an Fir with one node?
            // FIXME: Very ugly!
            Ok(value) => Ok(value.nodes.values().next().unwrap().data.0),
            Err(Incomplete { errs, .. }) => Err(Error::new(ErrKind::Multiple(errs))),
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;

    // #[test]
    // fn it_works() {
    //     let result = add(2, 2);
    //     assert_eq!(result, 4);
    // }
}
