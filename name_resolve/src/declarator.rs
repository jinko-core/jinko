use ast::{Ast, Node as AstNode};
use fir::{Fallible, Fir, Node, OriginIdx, RefIdx, Traversal};
use flatten::{AstInfo, FlattenData};

use crate::{NameResolutionError, NameResolveCtx, UniqueError};

enum DefinitionKind {
    Function,
    Type,
    Binding,
}

pub(crate) struct Declarator<'ctx, 'enclosing>(pub(crate) &'ctx mut NameResolveCtx<'enclosing>);

impl Declarator<'_, '_> {
    fn define(
        &mut self,
        kind: DefinitionKind,
        node: &Node<FlattenData>,
    ) -> Fallible<NameResolutionError> {
        let (map, kind) = match kind {
            DefinitionKind::Function => (&mut self.0.mappings.functions, "function"),
            DefinitionKind::Type => (&mut self.0.mappings.types, "type"),
            DefinitionKind::Binding => (&mut self.0.mappings.bindings, "binding"),
        };

        map.insert(
            node.data.ast.symbol().unwrap().clone(),
            node.origin,
            self.0.enclosing_scope[node.origin],
        )
        .map_err(|existing| Declarator::unique_error(node, existing, kind))
    }

    fn unique_error(
        node: &Node<FlattenData>,
        existing: OriginIdx,
        kind: &'static str,
    ) -> NameResolutionError {
        NameResolutionError::non_unique(node.data.ast.location(), UniqueError(existing, kind))
    }
}

impl<'ast> Traversal<FlattenData<'ast>, NameResolutionError> for Declarator<'_, '_> {
    // TODO: Can we factor these three functions?

    fn traverse_function(
        &mut self,
        _: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _: &[RefIdx],
        _: &[RefIdx],
        _: &Option<RefIdx>,
        _: &Option<RefIdx>,
    ) -> Fallible<NameResolutionError> {
        self.define(DefinitionKind::Function, node)
    }

    fn traverse_record_type(
        &mut self,
        _: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _: &[RefIdx],
        _: &[RefIdx],
    ) -> Fallible<NameResolutionError> {
        self.define(DefinitionKind::Type, node)
    }

    fn traverse_type_reference(
        &mut self,
        _fir: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        reference: &RefIdx,
    ) -> Fallible<NameResolutionError> {
        match node.data.ast {
            // if we're dealing with a type constant, then we have nothing to do during
            // name resolution (at least in the declaration pass)
            AstInfo::Node(Ast {
                node: AstNode::Constant(_),
                ..
            }) => Ok(()),
            _ => match reference {
                // if we already see resolved type references, then it means we are dealing
                // with a type alias
                RefIdx::Resolved(_) => self.define(DefinitionKind::Type, node),
                _ => Ok(()),
            },
        }
    }

    fn traverse_binding(
        &mut self,
        _: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _: &Option<RefIdx>,
        _: &Option<RefIdx>,
    ) -> Fallible<NameResolutionError> {
        self.define(DefinitionKind::Binding, node)
    }

    fn traverse_generic(
        &mut self,
        _: &Fir<FlattenData<'ast>>,
        node: &Node<FlattenData<'ast>>,
        _: &Option<RefIdx>,
    ) -> Fallible<NameResolutionError> {
        self.define(DefinitionKind::Type, node)
    }
}
