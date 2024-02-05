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

use crate::typemap::TypeMap;
use crate::{Type, TypeCtx};

// TODO: We need a way to fetch common type nodes used during typechecking, such as `bool` for example,
// as it's used for conditions for multiple nodes
pub(crate) struct Checker<'ctx>(pub(crate) &'ctx mut TypeCtx<TypeMap>);

impl<'ctx> Checker<'ctx> {
    fn get_type(&self, of: &RefIdx) -> &Type {
        // if at this point, the reference is unresolved, or if we haven't seen that node yet, it's
        // an interpreter error
        self.0
            .types
            .type_of(of.expect_resolved())
            .unwrap_or(self.unit())
    }

    fn unit(&self) -> &Type {
        // FIXME: Super ugly - rework
        self.0
            .types
            .types
            .get(&crate::typemap::TypeRef(self.0.primitives.unit_type))
            .unwrap()
    }

    fn expected_arithmetic_types(
        &self,
        loc: &SpanTuple,
        fir: &Fir<FlattenData>,
        op: builtins::Operator,
        args: &[RefIdx],
    ) -> Result<Vec<Type>, Error> {
        use builtins::*;

        // FIXME: Remove all calls to `::single` here?
        let numbers = [
            // FIXME: This is ugly
            RefIdx::Resolved(self.0.primitives.int_type),
            RefIdx::Resolved(self.0.primitives.float_type),
        ];
        let comparable = [
            RefIdx::Resolved(self.0.primitives.bool_type),
            RefIdx::Resolved(self.0.primitives.string_type),
            RefIdx::Resolved(self.0.primitives.int_type),
            RefIdx::Resolved(self.0.primitives.char_type),
            RefIdx::Resolved(self.0.primitives.float_type),
        ];

        let arity = match op {
            Operator::Arithmetic(_) | Operator::Comparison(_) => 2,
            Operator::Unary(_) => 1,
        };

        let valid_union_type = match op {
            Operator::Arithmetic(_) => Type::builtin(numbers.into_iter().collect()),
            Operator::Comparison(Equals) | Operator::Comparison(Differs) => {
                Type::builtin(comparable.into_iter().collect())
            }
            Operator::Unary(Minus) => Type::builtin(numbers.into_iter().collect()),
            // FIXME: These two are ugly as sin - remove the .map call
            Operator::Comparison(_) => {
                Type::builtin(comparable[2..].into_iter().map(|r| *r).collect())
            }
            Operator::Unary(Not) => {
                Type::builtin(comparable[..1].into_iter().map(|r| *r).collect())
            }
        };

        let expected_ty = match op.ty() {
            BuiltinType::Number | BuiltinType::Comparable => {
                self.get_type(&args[0]).clone() // FIXME: Remove clone
            }
            BuiltinType::Bool => Type::single(self.0.primitives.bool_type),
        };

        // FIXME: This API isn't great
        if valid_union_type.set().contains(&expected_ty.set()) {
            Ok(vec![expected_ty; arity])
        } else {
            Err(unexpected_arithmetic_type(
                loc,
                fir,
                &expected_ty, // FIXME: This is wrong
                &valid_union_type,
                op,
            ))
        }
    }
}

struct Fmt<'fir, 'ast>(&'fir Fir<FlattenData<'ast>>);

impl<'fir, 'ast> Fmt<'fir, 'ast> {
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

    // FIXME: Add debug format which adds new info like the RefIdx
    // is having a self parameter okay here
    pub fn ty(&self, ty: &Type) -> String {
        if ty.set().0.is_empty() {
            unreachable!()
        }

        ty.set()
            .0 // FIXME: ugly
            .iter()
            .map(|idx| self.0.nodes[&idx.expect_resolved()].data.ast.symbol())
            .fold(None, |acc, sym| match acc {
                None => Some(format!("`{}`", sym.unwrap().access().purple())),
                Some(acc) => Some(format!("{} | `{}`", acc, sym.unwrap().access().purple(),)),
            })
            .unwrap()
    }

    // #[deprecated(note = "unneeded?")]
    // pub fn ty_vec(tys: Vec<Option<String>>) -> String {
    //     tys.into_iter()
    //         // this calls `format::ty` but we are in the format module
    //         .map(ty)
    //         // FIXME: Improve formatting
    //         .fold(String::new(), |acc, ty| format!("{} | {}", acc, ty))
    // }
}

struct Expected<T>(T);
struct Got<T>(T);

fn type_mismatch(
    loc: &SpanTuple,
    fir: &Fir<FlattenData>,
    expected: Expected<&Type>,
    got: Got<&Type>,
) -> Error {
    let fmt = Fmt(fir);

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "type mismatch found: expected {}, got {}",
            fmt.ty(expected.0),
            fmt.ty(got.0)
        ))
        .with_loc(loc.clone()) // FIXME: Missing hint
}

fn argument_count_mismatch(loc: &SpanTuple, expected: Expected<usize>, got: Got<usize>) -> Error {
    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "argument count mismatch: expected {} {}, got {} {}",
            Fmt::number(expected.0),
            Fmt::plural("argument", expected.0),
            Fmt::number(got.0),
            Fmt::plural("argument", got.0),
        ))
        .with_loc(loc.clone())
    // FIXME: missing hint
}

fn unexpected_arithmetic_type(
    loc: &SpanTuple,
    fir: &Fir<FlattenData>,
    ty: &Type,
    valid_type_set: &Type, // FIXME: Should that be a Type?
    op: builtins::Operator,
) -> Error {
    let fmt = Fmt(fir);

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!(
            "unexpected type for arithmetic operation `{}`: `{}` (expected `{}`)",
            op.as_str().yellow(),
            fmt.ty(ty),
            fmt.ty(valid_type_set),
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

        let ret_ty = return_ty
            .as_ref()
            .map(|b| self.get_type(b))
            .unwrap_or(self.unit());
        let block_ty = block
            .as_ref()
            .map(|b| self.get_type(b))
            .unwrap_or(self.unit());

        if !block_ty.can_widen_to(ret_ty) {
            let err = type_mismatch(
                node.data.ast.location(),
                fir,
                Expected(ret_ty),
                Got(block_ty),
            );

            let err = match (return_ty, block) {
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
                    // FIXME: Remove clone
                    .map(|def_arg| self.get_type(def_arg).clone())
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
            .iter()
            .zip(args)
            .fold(Vec::new(), |mut errs, (expected, arg)| {
                let got = self.get_type(arg);

                // so we can't use != here anymore. multiple options
                // 1. instead of storing Option<Type>, keep Type - and add a new `None` variant to the Type
                // 2. this can be represented using an empty set, but we must then make sure that {} is "not" a subset of { int, string }
                // which makes very little sense in terms of set theory
                if !got.can_widen_to(expected) {
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

        // FIXME: Use `is_superset_of` here
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
