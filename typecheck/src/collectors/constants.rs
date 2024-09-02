//! Collect all primitive union type constants in the program in order to build our primitive union types properly. There are three primitive union types: `char`, `int` and `string`, so this module collects all character, integer and string constants.

use std::collections::HashSet;
use std::convert::Infallible;

use fir::{Fallible, Fir, Node, OriginIdx, RefIdx, Traversal};
use flatten::{AstInfo, FlattenData};

#[derive(Default)]
pub struct ConstantCollector {
    pub(crate) integers: HashSet<RefIdx>,
    pub(crate) characters: HashSet<RefIdx>,
    pub(crate) strings: HashSet<RefIdx>,
    pub(crate) floats: HashSet<RefIdx>,
    // Hopefully there's only two of those
    pub(crate) bools: HashSet<RefIdx>,
}

impl ConstantCollector {
    pub fn new() -> ConstantCollector {
        ConstantCollector::default()
    }

    fn add_integer(&mut self, idx: OriginIdx) {
        self.integers.insert(RefIdx::Resolved(idx));
    }

    fn add_character(&mut self, idx: OriginIdx) {
        self.characters.insert(RefIdx::Resolved(idx));
    }

    fn add_string(&mut self, idx: OriginIdx) {
        self.strings.insert(RefIdx::Resolved(idx));
    }

    fn add_bool(&mut self, idx: OriginIdx) {
        self.bools.insert(RefIdx::Resolved(idx));
    }

    fn add_float(&mut self, idx: OriginIdx) {
        self.floats.insert(RefIdx::Resolved(idx));
    }
}

impl Traversal<FlattenData<'_>, Infallible> for ConstantCollector {
    fn traverse_constant(
        &mut self,
        _: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        _: &RefIdx,
    ) -> Fallible<Infallible> {
        match node.data.ast {
            AstInfo::Node(ast::Ast {
                node: ast::Node::Constant(value),
                ..
            }) => match value {
                ast::Value::Integer(_) => self.add_integer(node.origin),
                ast::Value::Char(_) => self.add_character(node.origin),
                ast::Value::Str(_) => self.add_string(node.origin),
                ast::Value::Bool(_) => self.add_bool(node.origin),
                ast::Value::Float(_) => self.add_float(node.origin),
            },
            _ => unreachable!("Fir constant with non-node AST info. this is an interpreter error"),
        };

        Ok(())
    }
}
