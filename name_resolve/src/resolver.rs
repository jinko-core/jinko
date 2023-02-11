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
            ResolveKind::Var => write!(f, "var"),
        }
    }
}

pub(crate) struct Resolver<'ctx>(pub(crate) &'ctx mut NameResolveCtx);

impl<'ctx> Resolver<'ctx> {
    fn get_definition(
        &self,
        kind: ResolveKind,
        sym: &Option<Symbol>,
        location: &Option<SpanTuple>,
        scope: usize,
    ) -> Result<OriginIdx, NameResolutionError> {
        let symbol = sym
            .as_ref()
            .expect("attempting to get definition for non existent symbol - interpreter bug");

        let mappings = &self.0.mappings;
        let origin = match kind {
            ResolveKind::Call => mappings.get_function(symbol, scope),
            ResolveKind::Type => mappings.get_type(symbol, scope),
            ResolveKind::Var => mappings.get_variable(symbol, scope),
        };

        origin.map_or_else(
            || {
                Err(NameResolutionError::unresolved(
                    kind,
                    &self.0.mappings,
                    sym,
                    location,
                ))
            },
            |def| Ok(*def),
        )
    }
}

impl<'ctx> Mapper<FlattenData, FlattenData, NameResolutionError> for Resolver<'ctx> {
    fn map_call(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        to: RefIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
    ) -> Result<Node<FlattenData>, NameResolutionError> {
        // if there's no symbol, we're probably mapping a call to a function returned by a function
        // or similar, e.g `get_curried_fn(arg1)(arg2)`.
        match &data.symbol {
            None => Ok(Node {
                data,
                origin,
                kind: Kind::Call { to, generics, args },
            }),
            Some(_) => {
                let definition = self.get_definition(
                    ResolveKind::Call,
                    &data.symbol,
                    &data.location,
                    data.scope,
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

    fn map_typed_value(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        _value: RefIdx,
        ty: RefIdx,
    ) -> Result<Node<FlattenData>, NameResolutionError> {
        let definition =
            self.get_definition(ResolveKind::Var, &data.symbol, &data.location, data.scope)?;

        Ok(Node {
            data,
            origin,
            kind: Kind::TypedValue {
                value: RefIdx::Resolved(definition),
                ty,
            },
        })
    }

    fn map_type_reference(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        _reference: RefIdx,
    ) -> Result<Node<FlattenData>, NameResolutionError> {
        let definition =
            self.get_definition(ResolveKind::Type, &data.symbol, &data.location, data.scope)?;

        Ok(Node {
            data,
            origin,
            kind: Kind::TypeReference(RefIdx::Resolved(definition)),
        })
    }
}
