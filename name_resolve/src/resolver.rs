use std::{
    collections::HashMap,
    fmt::{Display, Formatter, Result as FmtResult},
};

use fir::{Kind, Mapper, Node, OriginIdx, RefIdx};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

use crate::{NameResolutionError, NameResolveCtx, Scope};

#[derive(Clone, Copy)]
pub(crate) enum ResolveKind {
    Call,
    Type,
    Binding,
}

impl Display for ResolveKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            ResolveKind::Call => write!(f, "call"),
            ResolveKind::Type => write!(f, "type"),
            ResolveKind::Binding => write!(f, "binding"),
        }
    }
}

pub(crate) struct Resolver<'ctx, 'enclosing> {
    pub(crate) ctx: &'ctx mut NameResolveCtx<'enclosing>,
    /// The `required_parents` vector contains information about origins whose resolution is necessary
    /// for the resolution of other nodes. For example, when resolving a type instantiation like
    /// `Foo(bar: 15)`, we need to have `Foo` resolved to its definition in order to resolve `bar` to
    /// that type's `.bar` field.
    // FIXME: Rework this part of the doc
    /// This vector is created as part of the first resolving pass - which
    /// we can call early-resolving, or straightforward-resolving, and will be used by a secondary,
    /// smaller resolving pass whose purpose is to map these undecidable usages to their definition.
    pub(crate) required_parents: Vec<OriginIdx>,
}

/// Created thanks to [`Resolver`]
pub(crate) struct LatentResolver<'ctx, 'enclosing> {
    pub(crate) ctx: &'ctx mut NameResolveCtx<'enclosing>,
    pub(crate) latent_map: HashMap<OriginIdx, Scope>,
}

impl<'ctx, 'enclosing> Resolver<'ctx, 'enclosing> {
    fn get_definition(
        &self,
        kind: ResolveKind,
        sym: Option<&Symbol>,
        location: &SpanTuple,
        node: OriginIdx,
    ) -> Result<OriginIdx, NameResolutionError> {
        let symbol =
            sym.expect("attempting to get definition for non existent symbol - interpreter bug");

        let mappings = &self.ctx.mappings;

        let scope = self.ctx.enclosing_scope[node];

        let origin = match kind {
            ResolveKind::Call => mappings.functions.lookup(symbol, scope),
            ResolveKind::Type => mappings.types.lookup(symbol, scope),
            ResolveKind::Binding => mappings.bindings.lookup(symbol, scope),
        };

        origin.map_or_else(
            || Err(NameResolutionError::unresolved(kind, sym, location)),
            |def| Ok(*def),
        )
    }
}

impl<'ast, 'ctx, 'enclosing> Mapper<FlattenData<'ast>, FlattenData<'ast>, NameResolutionError>
    for Resolver<'ctx, 'enclosing>
{
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

    fn map_typed_value(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        _value: RefIdx,
        ty: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, NameResolutionError> {
        let var_def = self.get_definition(
            ResolveKind::Binding,
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
        let ty = match &ty_def {
            Ok(def) => RefIdx::Resolved(*def),
            Err(_) => ty,
        };

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
            kind: Kind::TypedValue {
                value: RefIdx::Resolved(definition),
                ty,
            },
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

    fn map_assignment(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        to: RefIdx,
        from: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, NameResolutionError> {
        // we should probably do that ONLY if we can't resolve the binding?
        // that's very spaghetti...

        self.required_parents
            .push(self.ctx.enclosing_scope[origin].0);

        Ok(Node {
            data,
            origin,
            kind: Kind::Assignment { to, from },
        })
    }

    fn map_instantiation(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        _to: RefIdx,
        generics: Vec<RefIdx>,
        fields: Vec<RefIdx>,
    ) -> Result<Node<FlattenData<'ast>>, NameResolutionError> {
        // FIXME: Can we have _to be resolved already?

        let definition = self.get_definition(
            ResolveKind::Type,
            data.ast.symbol(),
            data.ast.location(),
            origin,
        )?;

        // do we have to go through all the fields here? should we instead have type fields count as declarations? e.g. encode them as <DefOriginIdx, Symbol>?

        Ok(Node {
            data,
            origin,
            kind: Kind::Instantiation {
                to: RefIdx::Resolved(definition),
                generics,
                fields,
            },
        })
    }
}

impl<'ast, 'ctx, 'enclosing> Mapper<FlattenData<'ast>, FlattenData<'ast>, NameResolutionError>
    for LatentResolver<'ctx, 'enclosing>
{
    // FIXME: Disgusting
    fn map_assignment(
        &mut self,
        data: FlattenData<'ast>,
        origin: OriginIdx,
        _: RefIdx,
        from: RefIdx,
    ) -> Result<Node<FlattenData<'ast>>, NameResolutionError> {
        // FIXME: Do nothing if _to is resolved already

        let instan = self.ctx.enclosing_scope[origin];
        let scope = self.latent_map[&instan.0];

        let bindings = self.ctx.mappings.bindings.scopes.get(&scope).unwrap();

        let resolved = bindings.get(data.ast.symbol().expect("interpreter error"));

        match resolved {
            Some(field) => Ok(Node {
                data,
                origin,
                kind: Kind::Assignment {
                    to: RefIdx::Resolved(*field),
                    from,
                },
            }),
            None => Err(NameResolutionError::Unresolved(
                ResolveKind::Binding,
                data.ast.symbol().unwrap().clone(),
                data.ast.location().clone(),
            )),
        }
    }
}
