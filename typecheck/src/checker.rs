//! Finally, after all nodes have been typed and simplified, we make sure that types are
//! valid. This includes checking that a function and its block return the same type, that
//! both branches of a condition return the same type, etc

use error::{ErrKind, Error};
use fir::{Fallible, Fir, Node, RefIdx, Traversal};
use flatten::FlattenData;
use location::SpanTuple;

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

mod format {
    use colored::Colorize;
    use symbol::Symbol;

    pub fn number(value: usize) -> String {
        match value {
            0 => format!("{}", "no".purple()),
            rest => format!("{}", rest.to_string().purple()),
        }
    }

    pub fn plural(to_pluralize: &str, value: usize) -> String {
        match value {
            1 => to_pluralize.to_string(),
            _ => format!("{to_pluralize}s"),
        }
    }

    pub fn ty(ty: Option<&Symbol>) -> String {
        match ty {
            Some(ty) => format!("`{}`", ty.access().purple()),
            None => format!("{}", "no type".green()),
        }
    }
}

struct Expected(Option<Type>);
struct Got(Option<Type>);

fn type_mismatch(loc: &SpanTuple, fir: &Fir<FlattenData>, expected: Expected, got: Got) -> Error {
    let get_symbol = |ty| {
        let Type::One(idx) = ty;
        fir.nodes[&idx.expect_resolved()].data.ast.symbol().unwrap()
    };

    let expected_ty = expected.0.map(get_symbol);
    let got_ty = got.0.map(get_symbol);

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "type mismatch found: expected {}, got {}",
            format::ty(expected_ty),
            format::ty(got_ty)
        ))
        .with_loc(loc.clone()) // FIXME: Missing hint
}

fn argument_count_mismatch(loc: &SpanTuple, expected: &[RefIdx], got: &[RefIdx]) -> Error {
    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "argument count mismatch: expected {} {}, got {} {}",
            format::number(expected.len()),
            format::plural("argument", expected.len()),
            format::number(got.len()),
            format::plural("argument", got.len()),
        ))
        .with_loc(loc.clone())
    // FIXME: missing hint
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
            let err = type_mismatch(
                node.data.ast.location(),
                fir,
                Expected(ret_ty),
                Got(block_ty),
            );
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

    fn traverse_call(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
        _generics: &[RefIdx],
        args: &[RefIdx],
    ) -> Fallible<Error> {
        let function = &fir.nodes[&to.expect_resolved()];
        let def_args = match &function.kind {
            fir::Kind::Function { args, .. } => args,
            _ => unreachable!("resolved call to a non-function. this is an interpreter error."),
        };

        if def_args.len() != args.len() {
            return Err(argument_count_mismatch(
                node.data.ast.location(),
                def_args,
                args,
            ));
        }

        // now we can safely zip both argument slices
        let errs = def_args
            .iter()
            .zip(args)
            .fold(Vec::new(), |mut errs, (def_arg, arg)| {
                let expected = self.get_type(def_arg);
                let got = self.get_type(arg);

                if expected != got {
                    errs.push(type_mismatch(
                        node.data.ast.location(),
                        fir,
                        Expected(expected),
                        Got(got),
                    ))
                }

                errs
            });

        if errs.is_empty() {
            Ok(())
        } else {
            Err(Error::new(ErrKind::Multiple(errs)))
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
            Err(type_mismatch(
                node.data.ast.location(),
                fir,
                Expected(to),
                Got(from),
            ))
        } else {
            Ok(())
        }
    }
}
