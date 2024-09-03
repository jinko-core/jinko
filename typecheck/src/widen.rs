use std::collections::{HashMap, HashSet};

use crate::typemap::{TypeMap, TypeRef};
use crate::{Type, TypeCtx, TypeSet};

use fir::{Fir, OriginIdx};
use flatten::FlattenData;

fn widen_inner(
    fir: &Fir<FlattenData<'_>>,
    type_ctx: &TypeCtx<TypeMap>,
    to_flatten: TypeSet,
) -> TypeSet {
    match to_flatten.0.len() {
        0 => TypeSet::empty(),
        1 => to_flatten,
        _ => {
            to_flatten
                .0
                .iter()
                .fold(TypeSet::empty(), |typeset, entry| {
                    // FIXME: Ugly, rework
                    let to_flatten = type_of(&type_ctx.types, entry.expect_resolved());

                    let set = match to_flatten {
                        Some(ty) => ty.set().clone(), // FIXME: No clone?
                        None => unreachable!(),
                    };

                    let new_set = widen_inner(fir, type_ctx, set);

                    typeset.merge(new_set)
                })
        }
    }
}

pub struct FlatTypeMap {
    pub(crate) type_map: TypeMap,
    pub(crate) original_types: HashSet<Type>,
}

fn type_of(type_map: &TypeMap, node: OriginIdx) -> Option<&Type> {
    type_map
        .nodes
        .get(&node)
        .and_then(|ty_ref| type_map.types.get(ty_ref))
        // in case that we are directly looking at the type's definition and not a reference.
        // this happens when dealing with inline type variables, such as union-types defined
        // in a function's return type.
        .or_else(|| type_map.types.get(&TypeRef(node)))
}

impl FlatTypeMap {
    fn new() -> FlatTypeMap {
        FlatTypeMap {
            type_map: TypeMap::new(),
            original_types: HashSet::new(),
        }
    }

    pub fn type_of(&self, node: OriginIdx) -> Option<&Type> {
        type_of(&self.type_map, node)
    }

    pub fn original_type_of(&self, ty: &Type) -> &Type {
        self.original_types.get(&ty).unwrap()
    }
}

pub fn widen(fir: &Fir<FlattenData<'_>>, type_ctx: TypeCtx<TypeMap>) -> TypeCtx<FlatTypeMap> {
    let mut types = type_ctx.types.types.iter().fold(
        FlatTypeMap::new(), /* NOTE: Can we take .nodes here? */
        |mut tymap, (_, ty)| {
            let origin = ty.origin();

            // FIXME: No clone in these two lines?

            tymap.original_types.insert(ty.clone());
            let set = widen_inner(fir, &type_ctx, ty.set().clone());

            let ty = Type(origin, set);

            tymap.type_map.new_type(ty);

            tymap
        },
    );

    // Don't forget to keep track of all the nodes that make use of the types
    // we just widened
    types.type_map.nodes = type_ctx.types.nodes;

    TypeCtx {
        primitives: type_ctx.primitives,
        types,
    }
}
