//! The Deduplication module aims at removing duplicate nodes from an [`Fir`]. This can be
//! done for "optimization" purposes, but not only - it is essential to type checking in order to
//! enable literal types and simplify union types.

use std::collections::HashMap;
use std::hash::{Hash, Hasher};

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

#[derive(PartialEq, Debug, Clone)]
struct FloatLiteral(f64);

impl Eq for FloatLiteral {}

impl Hash for FloatLiteral {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_bits().hash(state)
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
enum HashableValue<'ast> {
    Bool(bool),
    Char(char),
    Integer(i64),
    String(&'ast str),
    // NOTE: For some reason, it seems like using a `String` instead of `FloatLiteral` is
    // faster here, which makes me very sad. We can probably make this faster by putting constant
    // deduplication within the `Flatten` pass, but that's also very sad.
    Float(FloatLiteral),
}

impl<'ast> From<&'ast ast::Value> for HashableValue<'ast> {
    fn from(value: &'ast ast::Value) -> Self {
        use ast::Value::*;

        match value {
            Bool(b) => HashableValue::Bool(*b),
            Char(c) => HashableValue::Char(*c),
            Integer(i) => HashableValue::Integer(*i),
            Str(s) => HashableValue::String(s),
            Float(f) => HashableValue::Float(FloatLiteral(*f)),
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
