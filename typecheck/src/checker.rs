//! Finally, after all nodes have been typed and simplified, we make sure that types are
//! valid. This includes checking that a function and its block return the same type, that
//! both branches of a condition return the same type, etc

use colored::Colorize;
use error::{ErrKind, Error};
use fir::{Fallible, Fir, Node, RefIdx, Traversal};
use flatten::FlattenData;
use location::SpanTuple;

use std::iter::Iterator;

use builtins::Comparison::*;
use builtins::Unary::*;

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

    fn expected_arithmetic_types(
        &self,
        loc: &SpanTuple,
        fir: &Fir<FlattenData>,
        op: builtins::Operator,
        args: &[RefIdx],
    ) -> Result<Vec<Option<Type>>, Error> {
        use builtins::*;

        let numbers = [
            Type::One(RefIdx::Resolved(self.0.primitives.int_type)),
            Type::One(RefIdx::Resolved(self.0.primitives.float_type)),
        ];
        let comparable = [
            Type::One(RefIdx::Resolved(self.0.primitives.bool_type)),
            Type::One(RefIdx::Resolved(self.0.primitives.string_type)),
            Type::One(RefIdx::Resolved(self.0.primitives.int_type)),
            Type::One(RefIdx::Resolved(self.0.primitives.char_type)),
            Type::One(RefIdx::Resolved(self.0.primitives.float_type)),
        ];

        let arity = match op {
            Operator::Arithmetic(_) | Operator::Comparison(_) => 2,
            Operator::Unary(_) => 1,
        };

        let valid_types = match op {
            Operator::Arithmetic(_) => numbers.as_slice(),
            Operator::Comparison(Equals) | Operator::Comparison(Differs) => comparable.as_slice(),
            Operator::Comparison(_) => &comparable[2..],
            Operator::Unary(Not) => &comparable[..1],
            Operator::Unary(Minus) => &numbers[..1],
        };

        let expected_ty = match op.ty() {
            BuiltinType::Number | BuiltinType::Comparable => self.get_type(&args[0]).unwrap(),
            BuiltinType::Bool => Type::One(RefIdx::Resolved(self.0.primitives.bool_type)),
        };

        if valid_types.contains(&expected_ty) {
            Ok(vec![Some(expected_ty); arity])
        } else {
            Err(unexpected_arithmetic_type(
                loc,
                fir,
                &expected_ty,
                valid_types,
                op,
            ))
        }
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

    pub fn ty_vec(tys: Vec<Option<&Symbol>>) -> String {
        tys.into_iter()
            // this calls `format::ty` but we are in the format module
            .map(ty)
            // FIXME: Improve formatting
            .fold(String::new(), |acc, ty| format!("{} | {}", acc, ty))
    }
}

struct Expected<T>(T);
struct Got<T>(T);

fn type_mismatch(
    loc: &SpanTuple,
    fir: &Fir<FlattenData>,
    expected: Expected<Option<Type>>,
    got: Got<Option<Type>>,
) -> Error {
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

fn argument_count_mismatch(loc: &SpanTuple, expected: Expected<usize>, got: Got<usize>) -> Error {
    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "argument count mismatch: expected {} {}, got {} {}",
            format::number(expected.0),
            format::plural("argument", expected.0),
            format::number(got.0),
            format::plural("argument", got.0),
        ))
        .with_loc(loc.clone())
    // FIXME: missing hint
}

fn unexpected_arithmetic_type(
    loc: &SpanTuple,
    fir: &Fir<FlattenData>,
    ty: &Type,
    valid: &[Type],
    op: builtins::Operator,
) -> Error {
    let get_symbol = |ty| {
        let &Type::One(idx) = ty;
        fir.nodes[&idx.expect_resolved()].data.ast.symbol()
    };

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "unexpected type for arithmetic operation `{}`: {} (expected {})",
            op.as_str().yellow(),
            format::ty(get_symbol(ty)),
            format::ty_vec(valid.iter().map(get_symbol).collect()),
        ))
        .with_loc(loc.clone())
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

        // we need to special case arithmetic operators here, based on the data available in
        // the `builtins` package. their type is special, and their operator type is special as well - they also
        // need to be "coerced" to a specific type on return, we can't use the builtin number type
        let def_args = builtins::Operator::try_from_str(node.data.ast.symbol().unwrap().access())
            .map(|op| self.expected_arithmetic_types(node.data.ast.location(), fir, op, args))
            .unwrap_or_else(|| {
                Ok(def_args
                    .iter()
                    .map(|def_arg| self.get_type(def_arg))
                    .collect())
            })?;

        if def_args.len() != args.len() {
            return Err(argument_count_mismatch(
                node.data.ast.location(),
                Expected(def_args.len()),
                Got(args.len()),
            ));
        }

        // now we can safely zip both argument slices
        let errs = def_args
            .into_iter()
            .zip(args)
            .fold(Vec::new(), |mut errs, (expected, arg)| {
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
