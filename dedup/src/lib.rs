//! The Deduplication module aims at removing duplicate nodes from an [`Fir`]. This can be
//! done for "optimization" purposes, but not only - it is essential to type checking in order to
//! enable literal types and simplify union types.

use std::collections::HashMap;

use error::Error;
use fir::{Fir, Kind, Mapper, Node, OriginIdx, RefIdx};
use flatten::{AstInfo, FlattenData};

pub trait DeduplicateConstants: Sized {
    fn deduplicate_constants(self) -> Result<Self, Error>;
}

impl<'ast> DeduplicateConstants for Fir<FlattenData<'ast>> {
    fn deduplicate_constants(self) -> Result<Self, Error> {
        let mut ctx = ConstantDeduplicator(HashMap::new());

        // we can't emit an error in the constant deduplicator
        Ok(ctx.map(self).unwrap())
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum HashableValue<'ast> {
    Bool(bool),
    Char(char),
    Integer(i64),
    String(&'ast str),
}

impl<'ast> From<&'ast ast::Value> for HashableValue<'ast> {
    fn from(value: &'ast ast::Value) -> Self {
        use ast::Value::*;

        match value {
            Bool(b) => HashableValue::Bool(*b),
            Char(c) => HashableValue::Char(*c),
            Integer(i) => HashableValue::Integer(*i),
            Str(s) => HashableValue::String(s),
            Float(_) => {
                unreachable!("cannot hash floating point constants. this is an interpreter error")
            }
        }
    }
}

struct ConstantDeduplicator<'ast>(pub(crate) HashMap<HashableValue<'ast>, OriginIdx>);

impl<'ast> Mapper<FlattenData<'ast>, FlattenData<'ast>, Error> for ConstantDeduplicator<'ast> {
    fn map_constant(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        constant: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, Error> {
        match data.ast {
            // if we're dealing with a floating point constant, then we can't deduplicate - equality rules
            // are not great around floats and we can't use them as literal types anyway (and for that reason)
            AstInfo::Node(ast::Ast {
                node: ast::Node::Constant(ast::Value::Float(_)),
                ..
            }) => Ok(Node {
                origin,
                data,
                kind: Kind::Constant(constant),
            }),
            AstInfo::Node(ast::Ast {
                node: ast::Node::Constant(value),
                ..
            }) => {
                let value = HashableValue::from(value);

                match self.0.get(&value) {
                    // if the value was already present in the map, then we can just transform
                    // our constant into a reference to that constant
                    Some(constant) => Ok(Node {
                        origin,
                        data,
                        kind: Kind::NodeRef(RefIdx::Resolved(*constant)),
                    }),
                    // otherwise, we insert the value into the map and return this node as
                    // the "original constant", that future duplicate constants will refer to
                    None => {
                        self.0.insert(value, origin);

                        Ok(Node {
                            origin,
                            data,
                            kind: Kind::Constant(constant),
                        })
                    }
                }
            }
            _ => unreachable!(
                "constant without an AST constant as its node info. this is an interpreter error."
            ),
        }
    }
}
