// TODO: explain why this step is important as one could think we can just compare the results of Typer for equality, e.g. type_variable1 == type_variable2.
// TODO: but actually we need to be able to handle everything that the actual typesystem will rely on, such as subtyping

use std::ops::ControlFlow;

use error::Error;
use fir::{Fallible, Fir, Kind, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{typemap::TypeMap, Type, TypeCtx, TypeLinkMap, TypeVariable};

/// This pass takes care of shortening each link within the type context as much as possible. As explained in
/// the previous module ([`crate::typer`]), a list will be built within the [`TypeCtx`] containing "type links".
/// This pass will resolve these links and make each node point to its *actual* type.
pub(crate) struct Actual;

struct TypeLinkResolver<'ctx> {
    old: &'ctx TypeCtx<TypeLinkMap>,
    new: TypeCtx<TypeMap>,
}

impl Actual {
    pub fn resolve_type_links(
        ctx: &TypeCtx<TypeLinkMap>,
        fir: &Fir<FlattenData<'_>>,
    ) -> Result<TypeCtx<TypeMap>, Error> {
        let mut resolver = TypeLinkResolver {
            old: ctx,
            new: TypeCtx {
                primitives: ctx.primitives.clone(),
                types: TypeMap::new(),
            },
        };

        resolver.traverse(fir)?;

        Ok(resolver.new)
    }
}

struct ChainEnd {
    intermediate_nodes: Vec<OriginIdx>,
    final_type: Type,
}

impl TypeLinkResolver<'_> {
    /// Recursively try and resolve a type link within the type context. This will update the given node's type
    /// within the type context.
    fn resolve_link(&mut self, to_resolve: OriginIdx, fir: &Fir<FlattenData>) -> OriginIdx {
        dbg!(to_resolve);
        let ChainEnd {
            intermediate_nodes,
            final_type,
        } = self.find_end(fir, to_resolve);

        let node = final_type.origin();
        let tyref = self.new.types.new_type(final_type);

        intermediate_nodes
            .into_iter()
            .for_each(|node| self.new.types.insert(node, tyref));

        node
    }

    // FIXME: Rename
    // FIXME: Remove ResolutionCtx - too complicated for just one extra parameter
    fn find_end_inner(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        to_resolve: OriginIdx,
        mut intermediate_nodes: Vec<OriginIdx>,
    ) -> ControlFlow<ChainEnd, Vec<OriginIdx>> {
        let ty_of_node = self.old.types.get(&to_resolve).unwrap();

        match ty_of_node {
            TypeVariable::Reference(ty_ref) => {
                intermediate_nodes.push(to_resolve);

                self.find_end_inner(fir, ty_ref.expect_resolved(), intermediate_nodes)
            }
            TypeVariable::Generic(g) => ControlFlow::Break(ChainEnd {
                intermediate_nodes,
                final_type: Type::generic(*g),
            }),
            TypeVariable::Record(r) => {
                // we don't insert here so that we can get the typeref directly later on - does that make sense?
                // self.new.types.new_type(final_type.clone());

                ControlFlow::Break(ChainEnd {
                    intermediate_nodes,
                    final_type: Type::record(*r),
                })
            }
            TypeVariable::Union(u) => {
                let original_node = &fir.nodes[u];
                let variants = match &original_node.kind {
                    Kind::UnionType { variants, .. } => variants,
                    _ => unreachable!(),
                };

                // FIXME: This can cause an infinite loop - how to prevent that?
                let variants = variants
                    .iter()
                    .map(|variant| self.resolve_link(variant.expect_resolved(), fir))
                    .map(RefIdx::Resolved);

                // we don't insert here so that we can get the typeref directly later on - does that make sense?
                // self.new.types.new_type(final_type.clone());

                ControlFlow::Break(ChainEnd {
                    intermediate_nodes,
                    final_type: Type::union(*u, variants),
                })
            }
        }
    }

    fn find_end(&mut self, fir: &Fir<FlattenData<'_>>, to_resolve: OriginIdx) -> ChainEnd {
        let intermediate_nodes = vec![];

        match self.find_end_inner(fir, to_resolve, intermediate_nodes) {
            ControlFlow::Break(b) => b,
            ControlFlow::Continue(_) => unreachable!(),
        }
    }
}

impl Traversal<FlattenData<'_>, Error> for TypeLinkResolver<'_> {
    fn traverse_node(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
    ) -> Fallible<Error> {
        self.resolve_link(node.origin, fir);

        Ok(())
    }
}
