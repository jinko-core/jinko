use fir::{Fallible, Fir, Node, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{NameResolutionError, NameResolveCtx};

pub(crate) struct Declarator<'ctx>(pub(crate) &'ctx mut NameResolveCtx);

impl<'ctx> Traversal<FlattenData, NameResolutionError> for Declarator<'ctx> {
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
            .add_function(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| NameResolutionError::non_unique(&node.data.location, ue))
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
            .add_type(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| NameResolutionError::non_unique(&node.data.location, ue))
    }

    fn traverse_binding(
        &mut self,
        _fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _to: &RefIdx,
    ) -> Fallible<NameResolutionError> {
        self.0
            .mappings
            .add_variable(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| NameResolutionError::non_unique(&node.data.location, ue))
    }
}