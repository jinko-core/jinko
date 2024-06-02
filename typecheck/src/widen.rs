use std::any::Any;
use std::collections::HashSet;

use crate::TypeCtx;
use crate::{typemap::TypeMap, TypeSet};

use fir::{Fir, RefIdx};
use flatten::FlattenData;

fn widen_inner(
    fir: &Fir<FlattenData<'_>>,
    type_ctx: TypeCtx<TypeMap>,
    to_flatten: TypeSet,
) -> TypeSet {


    to_flatten.0.iter().fold(HashSet::new(), |typeset, entry| {
        let to_flatten = type_ctx.types.type_of(entry.expect_resolved());

        let set = match to_flatten {
            Some(ty) => ty.set(),
            None => unreachable!(),
        };
    })

    todo!()
}

pub fn widen(fir: &Fir<FlattenData<'_>>, type_ctx: TypeCtx<TypeMap>) -> TypeCtx<TypeMap> {
    type_ctx
        .types
        .types
        .iter()
        .map(|(k, v)| {
            let widened = v
                .set()
                .0
                .iter()
                .fold(HashSet::<RefIdx>::new(), |acc, ty| {
                    // FIXME: This needs to be a recursive function

                    type_ctx.types.type_of(k)
                })
                .collect();

            todo!()
        })
        .collect()
}
