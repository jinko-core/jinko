use crate::typemap::TypeMap;
use crate::{Type, TypeCtx, TypeSet};

fn widen_inner(type_ctx: &TypeCtx<TypeMap>, to_flatten: TypeSet) -> TypeSet {
    match to_flatten.0.len() {
        0 => TypeSet::empty(),
        1 => to_flatten,
        _ => {
            to_flatten
                .0
                .iter()
                .fold(TypeSet::empty(), |typeset, entry| {
                    let to_flatten = type_ctx.types.type_of(entry.expect_resolved());

                    let set = match to_flatten {
                        Some(ty) => ty.set().clone(), // FIXME: No clone?
                        None => unreachable!(),
                    };

                    let new_set = widen_inner(type_ctx, set);

                    typeset.merge(new_set)
                })
        }
    }
}

pub fn widen(type_ctx: TypeCtx<TypeMap>) -> TypeCtx<TypeMap> {
    let mut types = type_ctx
        .types
        .types
        .iter()
        .fold(TypeMap::new(), |mut tymap, (_, ty)| {
            let origin = ty.origin();
            let set = widen_inner(&type_ctx, ty.set().clone());

            let ty = Type(origin, set);

            tymap.new_type(ty);

            tymap
        });

    // Don't forget to keep track of all the nodes that make use of the types
    // we just widened
    types.nodes = type_ctx.types.nodes;

    TypeCtx {
        primitives: type_ctx.primitives,
        types,
    }
}
