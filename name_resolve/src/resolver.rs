use std::fmt::{Display, Formatter, Result as FmtResult};

use fir::{Kind, Mapper, Node, OriginIdx, RefIdx};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

use crate::{NameResolutionError, NameResolveCtx};

#[derive(Clone, Copy)]
pub(crate) enum ResolveKind {
    Call,
    Type,
    Var,
}

impl Display for ResolveKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            ResolveKind::Call => write!(f, "call"),
            ResolveKind::Type => write!(f, "type"),
            ResolveKind::Var => write!(f, "binding"),
        }
    }
}

pub(crate) struct Resolver<'ctx, 'enclosing>(pub(crate) &'ctx mut NameResolveCtx<'enclosing>);

impl Resolver<'_, '_> {
    fn get_definition(
        &self,
        kind: ResolveKind,
        sym: Option<&Symbol>,
        location: &SpanTuple,
        node: OriginIdx,
    ) -> Result<OriginIdx, NameResolutionError> {
        let symbol =
            sym.expect("attempting to get definition for non existent symbol - interpreter bug");

        let mappings = &self.0.mappings;

        let scope = self.0.enclosing_scope[node];

        let origin = match kind {
            ResolveKind::Call => mappings.functions.lookup(symbol, scope),
            ResolveKind::Type => mappings.types.lookup(symbol, scope),
            ResolveKind::Var => mappings.bindings.lookup(symbol, scope),
        };

        origin.map_or_else(
            || Err(NameResolutionError::unresolved(kind, sym, location)),
            |def| Ok(*def),
        )
    }
}

impl<'ast> Mapper<FlattenData<'ast>, FlattenData<'ast>, NameResolutionError> for Resolver<'_, '_> {
    fn map_call(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        to: RefIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
    ) -> Result<Node<FlattenData<'ast>>, NameResolutionError> {
        // if there's no symbol, we're probably mapping a call to a function returned by a function
        // or similar, e.g `get_curried_fn(arg1)(arg2)`.
        match &data.ast.symbol() {
            None => Ok(Node {
                data,
                origin,
                kind: Kind::Call { to, generics, args },
            }),
            Some(_) => {
                let definition = self.get_definition(
                    ResolveKind::Call,
                    data.ast.symbol(),
                    data.ast.location(),
                    origin,
                )?;

                Ok(Node {
                    data,
                    origin,
                    kind: Kind::Call {
                        to: RefIdx::Resolved(definition),
                        generics,
                        args,
                    },
                })
            }
        }
    }

    fn map_node_ref(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        _: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, NameResolutionError> {
        let var_def = self.get_definition(
            ResolveKind::Var,
            data.ast.symbol(),
            data.ast.location(),
            origin,
        );
        let ty_def = self.get_definition(
            ResolveKind::Type,
            data.ast.symbol(),
            data.ast.location(),
            origin,
        );

        // If we're dealing with a type definition, we can "early typecheck"
        // to the empty type's definition
        // FIXME: Leave that to the actual typechecker
        // let ty = match &ty_def {
        //     Ok(def) => RefIdx::Resolved(*def),
        //     Err(_) => ty,
        // };

        let definition = match (var_def, ty_def) {
            (Ok(var_def), Ok(ty_def)) => Err(NameResolutionError::ambiguous_binding(
                var_def,
                ty_def,
                data.ast.location(),
            )),
            (Err(_), Err(_)) => Err(NameResolutionError::unresolved_binding(
                data.ast.symbol(),
                data.ast.location(),
            )),
            (Ok(def), _) | (_, Ok(def)) => Ok(def),
        }?;

        Ok(Node {
            data,
            origin,
            kind: Kind::NodeRef(RefIdx::Resolved(definition)),
        })
    }

    fn map_type_reference(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        reference: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, NameResolutionError> {
        // short circuit in case the type-reference was already resolved - this can happen for
        // things like type aliases where we can create type references during flattening
        if let RefIdx::Resolved(_) = reference {
            Ok(Node {
                data,
                origin,
                kind: Kind::TypeReference(reference),
            })
        } else {
            let definition = self.get_definition(
                ResolveKind::Type,
                data.ast.symbol(),
                data.ast.location(),
                origin,
            )?;

            Ok(Node {
                data,
                origin,
                kind: Kind::TypeReference(RefIdx::Resolved(definition)),
            })
        }
    }
}
