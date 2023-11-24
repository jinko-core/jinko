use error::Error;
use fir::{Fallible, Fir, Kind, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

use crate::{typemap::TypeMap, Type, TypeCtx, TypeLinkMap, TypeVariable};

/// This pass takes care of shortening each link within the type context as much as possible. As explained in
/// the previous module ([`crate::typer`]), a list will be built within the [`TypeCtx`] containing "type links".
/// This pass will resolve these links and make each node point to its *actual* type.
pub(crate) struct Actual; // <'ctx>(pub(crate) &'ctx mut TypeCtx);

// what should the interface look like so that we can have
// let typectx = Actual(typectx).traverse(fir);

fn resolve_type_chain_RENAME_ME(
    ctx: &TypeCtx<TypeLinkMap>,
    new: &mut TypeCtx<TypeMap>,
    fir: &Fir<FlattenData<'_>>,
    linked_node: RefIdx,
    // this should probably return the Origin as well? so we can do some magic with the node after
) {
    // FIXME: Remove mutability, make it more functional
    let ty = ctx.types.get(&linked_node.expect_resolved()).unwrap();

    // so accumulate type nodes as we go through the chain, then walk everything back setting TypeRef?
    // when we do find a type (so Kind == UnionType | RecordType), we insert the new type in the type map.
    // on each step we should check if we have typemap.contains(node), and if we do, copy the typeref and exit the loop
    // this returns a TypeRef with which we insert for all the nodes in the chain?
    // this function should probably be renamed

    // FIXME: This API does not work with what we've done so far
    loop {
        let intermediate_nodes = vec![];

        match ty {
            None => todo!(),
            Some(TypeVariable::Reference(referenced_ty)) => intermediate_nodes.push(referenced_ty),
            Some(TypeVariable::Actual(ty)) => {}
        }
    }
}

fn innermost_type(
    ctx: &TypeCtx<TypeLinkMap>,
    fir: &Fir<FlattenData<'_>>,
    linked_node: RefIdx,
    // this should probably return the Origin as well? so we can do some magic with the node after
) -> Option<Type> {
    let linked_node = linked_node.expect_resolved();

    // how do we improve that?
    // make it functional/recursive?
    // then handle TypeReference/RecordType/UnionType?

    // so can we replace all of this by a permanent lookup until we find a Type::Actual?
    // probably, right?
    // and we won't need the FIR as a parameter anymore
    let ty = ctx.types.get(&linked_node);

    // we get the typevariable here - if it does not exist, we should error out because that means this node hasn't been typed at all which is an error
    // then we look at it. we kinda want to iterate until we find a Union/Record, and then walk back the chain maybe?

    // when we find a Union/Record, we create a Type and a TypeRef, insert them into the TypeMap
    // actually I don't think that's how it works :(

    match ty {
        Some(ty) => match ty {
            Some(ty) => match ty {
                // This is an issue - we can run into situations where we have an `Actual` which
                // points to an `fir::Kind::TypeReference` - so we must extract that
                TypeVariable::Actual(ty) => {
                    if ty.set().0.len() == 1 {
                        // If we have only one type in the typeset, it might be a typereference - if that is the case,
                        // `innermost_type` it again
                        let referenced_ty = ty.set().0.iter().next().unwrap();
                        let referenced_node = &fir.nodes[&referenced_ty.expect_resolved()];

                        match &referenced_node.kind {
                            Kind::TypeReference(r) => innermost_type(ctx, fir, *r),
                            Kind::RecordType { .. } => {
                                Some(Type::single(RefIdx::Resolved(referenced_node.origin)))
                            }
                            Kind::UnionType { variants, .. } => Some(Type::new(
                                variants
                                    .into_iter()
                                    .map(|variant| innermost_type(ctx, fir, *variant))
                                    .map(|variant| {
                                        variant.unwrap().set().0.iter().copied().next().unwrap()
                                    })
                                    .collect(),
                            )),
                            _ => unreachable!(),
                        }
                    } else {
                        Some(Type::new(
                            ty.set()
                                .0
                                .iter()
                                .map(|variant| innermost_type(ctx, fir, *variant))
                                .map(|variant| {
                                    variant.unwrap().set().0.iter().copied().next().unwrap()
                                })
                                .collect(),
                        ))
                    }
                    // if let Kind::TypeReference(r) = referenced_node.kind {
                    //     innermost_type(ctx, fir, r)
                    // } else {
                    //     // FIXME: Remove clone
                    //     Some(ty.clone())
                    // }
                }
                TypeVariable::Reference(r) => innermost_type(ctx, fir, *r),
            },
            None => return None,
        },
        None => todo!(),
    }

    //     // if the context contains an entry for `linked_node`, then we use this - otherwise, we simply
    //     // get the node from the FIR
    //     let to_lookup = &ctx
    //         .types
    //         .get(&linked_node)
    //         .and_then(|ty| ty.map(|t| t.ref_idx().expect_resolved()))
    //         .unwrap_or(linked_node);

    //     let linked_node = &fir.nodes[to_lookup];

    //     let inner_opt = |fir, opt| match opt {
    //         Some(opt) => innermost_type(ctx, fir, opt),
    //         None => None,
    //     };

    //     match &linked_node.kind {
    //         // shouldn't we instead stop when we find a TypeVariable::Actual?
    //         // these are the *only* terminal branches
    //         Kind::RecordType { .. } | Kind::UnionType { .. } => {
    //             Some(Type::One(RefIdx::Resolved(linked_node.origin)))
    //         }
    //         Kind::Assignment { .. } => None,
    //         // if a typed value has no specific type, but points to a value, use this as the source of the type
    //         Kind::TypedValue {
    //             ty: RefIdx::Unresolved,
    //             value,
    //             // can we set `ty` here or not? probably not
    //             // we need to :(
    //         } => innermost_type(ctx, fir, *value),
    //         Kind::Constant(ty)
    //         | Kind::TypeReference(ty)
    //         | Kind::TypedValue { ty, .. }
    //         | Kind::Binding { to: ty }
    //         | Kind::Instantiation { to: ty, .. }
    //         | Kind::Call { to: ty, .. }
    //         | Kind::Conditional { true_block: ty, .. } => innermost_type(ctx, fir, *ty),
    //         Kind::Function {
    //             return_type: ty, ..
    //         }
    //         | Kind::Return(ty) => inner_opt(fir, *ty),
    //         Kind::Statements(stmts) => inner_opt(fir, stmts.last().copied()),
    //         // FIXME: What to do here?
    //         Kind::Generic { .. } => todo!(),
    //         Kind::Loop { .. } => todo!(),
    //         Kind::TypeOffset { .. } => todo!(),
    //     }
}

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

impl<'ctx> TypeLinkResolver<'ctx> {
    /// Recursively try and resolve a type link within the type context. This will update the given node's type
    /// within the type context.
    fn resolve_link(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        to_resolve: OriginIdx,
    ) -> Fallible<Error> {
        // FIXME: Remove mutability, make it more functional

        // so accumulate type nodes as we go through the chain, then walk everything back setting TypeRef?
        // when we do find a type (so Kind == UnionType | RecordType), we insert the new type in the type map.
        // on each step we should check if we have typemap.contains(node), and if we do, copy the typeref and exit the loop
        // this returns a TypeRef with which we insert for all the nodes in the chain?
        // this function should probably be renamed

        let mut intermediate_nodes = vec![];
        let mut current = to_resolve;

        // FIXME: This API does not work with what we've done so far
        loop {
            // if any node has not been through "Typer" before, this is an interpreter error
            let ty = self.old.types.get(&current).unwrap();

            match ty {
                None => todo!(),
                Some(TypeVariable::Reference(referenced_ty)) => {
                    let node = referenced_ty.expect_resolved();

                    intermediate_nodes.push(node);
                    current = node;
                }
                Some(TypeVariable::Actual(ty)) => {
                    if ty.set().0.len() == 1 {
                        // If we have only one type in the typeset, it might be a typereference - if that is the case,
                        // `innermost_type` it again
                        // FIXME: This needs more explaining
                        let referenced_ty = ty.set().0.iter().next().unwrap();
                        let node = referenced_ty.expect_resolved();

                        let kind = fir[&node].kind;

                        match kind {
                            Kind::TypeReference(r) => {
                                intermediate_nodes.push(node);
                                current = r.expect_resolved();
                            }
                            // we found an actual type definition - insert it, break the loop and type
                            // all the intermediate nodes
                            Kind::RecordType { fields, .. } => {
                                let ty = Type::single(RefIdx::Resolved(node));

                                self.new.types.new_type(node, ty);
                            }
                            Kind::UnionType { variants, .. } => {
                                todo!()
                                // FIXME: How do we get the actual type here? we need to go through innermost_type again? :(
                                // how are types represented in that case? Type::Set { OriginIdx, OriginIdx, OriginIdx }?
                                // so we need to have these nodes typed already and to get their type from the typemap?
                                //         variants
                                //             .into_iter()
                                //             .map(|variant| innermost_type(ctx, fir, *variant))
                                //             .map(|variant| {
                                //                 variant.unwrap().set().0.iter().copied().next().unwrap()
                                //             })
                                //             .collect(),
                            }
                            _ => unreachable!(),
                        }

                        continue;
                    }

                    // FIXME: Remove
                    break;
                    // let referenced_node = &fir.nodes[&referenced_ty.expect_resolved()];

                    // match &ty.kind {
                    //     Kind::TypeReference(r) => innermost_type(ctx, fir, *r),
                    //     Kind::RecordType { .. } => {
                    //         Some(Type::single(RefIdx::Resolved(referenced_node.origin)))
                    //     }
                    //     Kind::UnionType { variants, .. } => Some(Type::new(
                    //         variants
                    //             .into_iter()
                    //             .map(|variant| innermost_type(ctx, fir, *variant))
                    //             .map(|variant| {
                    //                 variant.unwrap().set().0.iter().copied().next().unwrap()
                    //             })
                    //             .collect(),
                    //     )),
                    //     _ => unreachable!(),
                    // }
                }
            }
        }

        Ok(())
    }
}

impl<'ctx> Traversal<FlattenData<'_>, Error> for TypeLinkResolver<'ctx> {
    fn traverse_node(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
    ) -> Fallible<Error> {
        self.resolve_link(fir, node.origin)
    }
}
