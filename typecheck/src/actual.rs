use error::Error;
use fir::{Fallible, Fir, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{Type, TypeCtx};

/// This pass takes care of shortening each link within the type context as much as possible. As explained in
/// the previous module ([`crate::typer`]), a list will be built within the [`TypeCtx`] containing "type links".
/// This pass will resolve these links and make each node point to its *actual* type.
pub(crate) struct Actual(pub(crate) TypeCtx);

impl Actual {
    /// Recursively try and resolve a type link within the type context
    fn resolve_link(&mut self, fir: &Fir<FlattenData>, to_resolve: &OriginIdx) -> Fallible<Error> {
        // if any node has not been through "Typer" before, this is an interpreter error
        let link = self.0.types.get(to_resolve).unwrap();

        // void nodes are already fully typed, so we don't take care of them
        if let Type::One(ty_ref) = link {
            let link_node = fir.nodes[ty_ref];
        }

        Ok(())
    }
}

impl Traversal<FlattenData, Error> for Actual {}
