use crate::typemap::TypeMap;
use crate::{Type, TypeCtx, TypeSet};

use fir::Fir;
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
                    let to_flatten = type_ctx.types.type_of(entry.expect_resolved());

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

pub fn widen(fir: &Fir<FlattenData<'_>>, type_ctx: TypeCtx<TypeMap>) -> TypeCtx<TypeMap> {
    let types = type_ctx
        .types
        .types
        .iter()
        .fold(TypeMap::new(), |mut tymap, (_, ty)| {
            let origin = ty.origin();
            let set = widen_inner(fir, &type_ctx, ty.set().clone());

            let ty = Type(origin, set);

            tymap.new_type(ty);

            tymap
        });

    TypeCtx {
        primitives: type_ctx.primitives,
        types,
    }
}
