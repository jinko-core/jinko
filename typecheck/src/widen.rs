use std::collections::HashSet;

use crate::typemap::TypeMap;
use crate::TypeCtx;

use fir::{Fir, RefIdx};
use flatten::FlattenData;

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
