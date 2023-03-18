//! Finally, after all nodes have been typed and simplified, we make sure that types are
//! valid. This includes checking that a function and its block return the same type, that
//! both branches of a condition return the same type, etc

use error::{ErrKind, Error};
use fir::{Fallible, Fir, Node, RefIdx, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

use crate::{Type, TypeCtx};

// TODO: We need a way to fetch common type nodes used during typechecking, such as `bool` for example,
// as it's used for conditions for multiple nodes
pub(crate) struct Checker<'ctx>(pub(crate) &'ctx mut TypeCtx);

impl<'ctx> Checker<'ctx> {
    fn get_type(&self, of: &RefIdx) -> Option<Type> {
        match of {
            RefIdx::Resolved(of) => *self.0.types.get(of).unwrap(),
            RefIdx::Unresolved => unreachable!("unresolved node in `Fir`"),
        }
    }
}

fn type_mismatch(
    loc: &Option<SpanTuple>,
    fir: &Fir<FlattenData>,
    expected: Option<Type>,
    got: Option<Type>,
) -> Error {
    let void = Symbol::from("void");
    let get_symbol = |ty| match ty {
        Type::One(RefIdx::Resolved(idx)) => fir.nodes[&idx].data.symbol.clone().unwrap(),
        _ => unreachable!("unresolved pattern"),
    };

    let expected_ty = expected.map_or(void.clone(), get_symbol);
    let got_ty = got.map_or(void, get_symbol);

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "type mismatch found: expected `{expected_ty}`, got `{got_ty}`"
        ))
        .with_loc(loc.clone()) // FIXME: Missing hint
}

impl<'ctx> Traversal<FlattenData, Error> for Checker<'ctx> {
    fn traverse_function(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        return_ty: &Option<RefIdx>,
        block: &Option<RefIdx>,
    ) -> Fallible<Error> {
        let ret_ty = return_ty.as_ref().and_then(|b| self.get_type(b));
        let block_ty = block.as_ref().and_then(|b| self.get_type(b));

        match (ret_ty, block_ty) {
            // All functions which do not have a return type are by definition
            // typed correctly
            (_, None) => Ok(()),
            // if a function should return void, but returns
            (ret_ty, block_ty) => {
                if ret_ty != block_ty {
                    Err(type_mismatch(&node.data.location, fir, ret_ty, block_ty))
                } else {
                    Ok(())
                }
            }
        }
    }
}
