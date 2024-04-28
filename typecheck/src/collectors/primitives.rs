//! First, we need to fetch the primitive types from the current source, and error out if
//! they are not present. This module uses typestates to ensure that we only provide a valid
//! set of primitives, or return an error

use error::{ErrKind, Error};
use fir::{Fallible, Fir, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

/// All primitive types that must be found before we actually perform typechecking
#[derive(Default)]
struct PrimitiveTypeCtx {
    unit_type: Option<OriginIdx>,
    bool_type: Option<OriginIdx>,
    char_type: Option<OriginIdx>,
    int_type: Option<OriginIdx>,
    float_type: Option<OriginIdx>,
    string_type: Option<OriginIdx>,
}

/// This is the finalized version of [`PrimitiveTypeCtx`] - a version which only has
/// valid [`OriginIdx`], or otherwise it will not be returned, and an [`Error`] will
/// be present instead
#[derive(Clone)]
pub struct PrimitiveTypes {
    pub(crate) unit_type: OriginIdx,
    pub(crate) bool_type: OriginIdx,
    pub(crate) char_type: OriginIdx,
    pub(crate) int_type: OriginIdx,
    pub(crate) float_type: OriginIdx,
    pub(crate) string_type: OriginIdx,
}

fn validate_type(
    fir: &Fir<FlattenData<'_>>,
    sym: &Symbol,
    loc: &SpanTuple,
    generics: &[RefIdx],
    fields: &[RefIdx],
) -> Fallible<Error> {
    let collect_locs = |many_refs: &[RefIdx]| {
        many_refs
            .iter()
            .map(|generic| &fir.nodes[&generic.expect_resolved()])
            .map(|node| node.data.ast.location().clone())
            .collect::<Vec<SpanTuple>>()
    };

    let maybe_err = |many_refs: &[RefIdx], kind: &str, to_remove: &str| {
        (!many_refs.is_empty()).then_some({
            let err = Error::new(ErrKind::TypeChecker)
                .with_msg(format!("primitive type `{sym}` was declared with {kind}"))
                .with_loc(loc.clone());

            let locs = collect_locs(generics);

            locs.into_iter().fold(err, |err, loc| {
                err.with_hint(
                    Error::hint()
                        .with_msg(format!("remove this {to_remove}"))
                        .with_loc(loc),
                )
            })
        })
    };

    let generic_error = maybe_err(generics, "generics", "generic parameter");
    let field_error = maybe_err(fields, "fields", "field");

    match (generic_error, field_error) {
        (None, None) => Ok(()),
        (Some(e1), Some(e2)) => Err(Error::new(ErrKind::Multiple(vec![e1, e2]))),
        (Some(e), _) | (_, Some(e)) => Err(e),
    }
}

fn duplicate(fir: &Fir<FlattenData<'_>>, old: &OriginIdx, new: &OriginIdx) -> Error {
    let old = &fir.nodes[old].data.ast;

    let name = old.symbol().unwrap();
    let old_loc = old.location().clone();
    let new_loc = fir.nodes[new].data.ast.location().clone();

    Error::new(ErrKind::TypeChecker)
        .with_msg(format!("re-declaration of primitive type {name}"))
        .with_hint(
            Error::hint()
                .with_msg(format!("primitive type {name} previously declared here"))
                .with_loc(old_loc),
        )
        .with_hint(Error::hint().with_msg(String::from(
            "re-defining primitive types in a different scope is not allowed",
        )))
        .with_loc(new_loc)
}

impl Traversal<FlattenData<'_>, Error> for PrimitiveTypeCtx {
    fn traverse_record_type(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        generics: &[RefIdx],
        fields: &[RefIdx],
    ) -> Fallible<Error> {
        let ast = &node.data.ast;
        // Type declarations always have a symbol, we can unwrap safely, or this is an
        // interpreter error
        let name = ast.symbol().unwrap();

        let field_to_set = match name.access() {
            "unit" => Some(&mut self.unit_type),
            "bool" => Some(&mut self.bool_type),
            "char" => Some(&mut self.char_type),
            "int" => Some(&mut self.int_type),
            "float" => Some(&mut self.float_type),
            "string" => Some(&mut self.string_type),
            _ => None,
        };

        if let Some(field) = field_to_set {
            validate_type(fir, name, ast.location(), generics, fields)?;

            if let Some(existing) = *field {
                Err(duplicate(fir, &existing, &node.origin))
            } else {
                *field = Some(node.origin);

                Ok(())
            }
        } else {
            Ok(())
        }
    }
}

fn extract_types(ctx: &PrimitiveTypeCtx) -> Result<PrimitiveTypes, Error> {
    let missing_ty = |name| {
        move || {
            Error::new(ErrKind::TypeChecker).with_msg(format!(
                "missing implementation for primitive type `{name}`",
            ))
        }
    };

    // FIXME: we need to do a fold here and accumulate errors
    let unit_type = ctx.unit_type.ok_or_else(missing_ty("unit"))?;
    let bool_type = ctx.bool_type.ok_or_else(missing_ty("bool"))?;
    let char_type = ctx.char_type.ok_or_else(missing_ty("char"))?;
    let int_type = ctx.int_type.ok_or_else(missing_ty("int"))?;
    let float_type = ctx.float_type.ok_or_else(missing_ty("float"))?;
    let string_type = ctx.string_type.ok_or_else(missing_ty("string"))?;

    Ok(PrimitiveTypes {
        unit_type,
        bool_type,
        char_type,
        int_type,
        float_type,
        string_type,
    })
}

pub fn find(fir: &Fir<FlattenData<'_>>) -> Result<PrimitiveTypes, Error> {
    let mut ctx = PrimitiveTypeCtx::default();

    ctx.traverse(fir)?;

    extract_types(&ctx)
}

#[cfg(test)]
mod tests {
    use flatten::FlattenAst;
    use name_resolve::NameResolve;
    use xparser::ast;

    #[test]
    fn full_decls() {
        let ast = ast! {
            type unit;

            type true;
            type false;
            type bool;

            type char;
            type int;
            type float;

            type string;
        };

        let fir = ast.flatten().name_resolve().unwrap();

        assert!(super::find(&fir).is_ok())
    }

    #[test]
    fn missing_ty() {
        let ast = ast! {
            type true;
            type false;
            type bool;

            type string;
        };

        let fir = ast.flatten().name_resolve().unwrap();

        assert!(super::find(&fir).is_err())
    }

    #[test]
    fn invalid_declaration() {
        let ast = ast! {
            type true;
            type false;
            type bool;

            type char;
            type int;
            type float;

            type string(data: char, next: char, size: int);
        };

        let fir = ast.flatten().name_resolve().unwrap();

        assert!(super::find(&fir).is_err())
    }
}
