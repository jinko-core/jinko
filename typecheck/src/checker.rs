//! Finally, after all nodes have been typed and simplified, we make sure that types are
//! valid. This includes checking that a function and its block return the same type, that
//! both branches of a condition return the same type, etc

use error::{ErrKind, Error};
use fir::{Fallible, Fir, Node, RefIdx, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

use colored::Colorize;

use crate::{Type, TypeCtx};

// TODO: We need a way to fetch common type nodes used during typechecking, such as `bool` for example,
// as it's used for conditions for multiple nodes
pub(crate) struct Checker<'ctx>(pub(crate) &'ctx mut TypeCtx);

impl<'ctx> Checker<'ctx> {
    fn get_type(&self, of: &RefIdx) -> Option<Type> {
        // if at this point, the reference is unresolved, or if we haven't seen that node yet, it's
        // an interpreter error
        *self.0.types.get(&of.expect_resolved()).unwrap()
    }
}

fn type_mismatch(
    loc: &SpanTuple,
    fir: &Fir<FlattenData>,
    expected: Option<Type>,
    got: Option<Type>,
) -> Error {
    let get_symbol = |ty| {
        let Type::One(idx) = ty;
        fir.nodes[&idx.expect_resolved()].data.ast.symbol().unwrap()
    };
    let name_fmt = |ty: Option<&Symbol>| match ty {
        Some(ty) => format!("`{}`", ty.access().purple()),
        None => format!("{}", "no type".green()),
    };

    let expected_ty = expected.map(get_symbol);
    let got_ty = got.map(get_symbol);

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "type mismatch found: expected {}, got {}",
            name_fmt(expected_ty),
            name_fmt(got_ty)
        ))
        .with_loc(loc.clone()) // FIXME: Missing hint
}

impl<'ctx> Traversal<FlattenData<'_>, Error> for Checker<'ctx> {
    fn traverse_function(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        return_ty: &Option<RefIdx>,
        block: &Option<RefIdx>,
    ) -> Fallible<Error> {
        // if there is no block, e.g. an extern function, then we trust the return type
        if block.is_none() {
            return Ok(());
        }

        let ret_ty = return_ty.as_ref().and_then(|b| self.get_type(b));
        let block_ty = block.as_ref().and_then(|b| self.get_type(b));

        if ret_ty != block_ty {
            let err = type_mismatch(node.data.ast.location(), fir, ret_ty, block_ty);
            let err = match (ret_ty, block_ty) {
                (None, Some(_)) => err
                    .with_hint(Error::hint().with_msg(String::from(
                        "this function is not expected to return any value but does",
                    )))
                    .with_hint(Error::hint().with_msg(String::from(
                        "you might have meant to ignore the last expression?",
                    ))),
                (Some(_), None) => err
                    .with_hint(Error::hint().with_msg(String::from(
                        "this function's block does not return any value",
                    )))
                    .with_hint(Error::hint().with_msg(String::from(
                        "you might have added an extra semicolon or forgotten an expression?",
                    ))),
                _ => err,
            };

            Err(err)
        } else {
            Ok(())
        }
    }

    fn traverse_assignment(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        to: &RefIdx,
        from: &RefIdx,
    ) -> Fallible<Error> {
        let to = self.get_type(to);
        let from = self.get_type(from);

        if to != from {
            Err(type_mismatch(node.data.ast.location(), fir, to, from))
        } else {
            Ok(())
        }
    }
}
