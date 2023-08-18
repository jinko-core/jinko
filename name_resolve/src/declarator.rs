use fir::{Fallible, Fir, Node, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{NameResolutionError, NameResolveCtx, UniqueError};

pub(crate) struct Declarator<'ctx, 'enclosing>(pub(crate) &'ctx mut NameResolveCtx<'enclosing>);

impl<'ast, 'ctx, 'enclosing> Traversal<FlattenData<'ast>, NameResolutionError>
    for Declarator<'ctx, 'enclosing>
{
    fn traverse_function(
        &mut self,
        _fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        _return_ty: &Option<RefIdx>,
        _block: &Option<RefIdx>,
    ) -> Fallible<NameResolutionError> {
        self.0
            .mappings
            .functions
            .insert(
                node.data.ast.symbol().unwrap().clone(),
                node.origin,
                self.0.enclosing_scope[node.origin],
            )
            .map_err(|existing| {
                NameResolutionError::non_unique(
                    node.data.ast.location(),
                    UniqueError(existing, "function"),
                )
            })
    }

    fn traverse_type(
        &mut self,
        _fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _: &[RefIdx],
        _: &[RefIdx],
    ) -> Fallible<NameResolutionError> {
        self.0
            .mappings
            .types
            .insert(
                node.data.ast.symbol().unwrap().clone(),
                node.origin,
                self.0.enclosing_scope[node.origin],
            )
            .map_err(|existing| {
                NameResolutionError::non_unique(
                    node.data.ast.location(),
                    UniqueError(existing, "type"),
                )
            })
    }

    fn traverse_binding(
        &mut self,
        _fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _to: &RefIdx,
    ) -> Fallible<NameResolutionError> {
        self.0
            .mappings
            .variables
            .insert(
                node.data.ast.symbol().unwrap().clone(),
                node.origin,
                self.0.enclosing_scope[node.origin],
            )
            .map_err(|existing| {
                NameResolutionError::non_unique(
                    node.data.ast.location(),
                    UniqueError(existing, "binding"),
                )
            })
    }
}
