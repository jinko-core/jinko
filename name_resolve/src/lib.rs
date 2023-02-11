use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result as FmtResult};

use error::{ErrKind, Error};
use fir::{Fallible, Fir, IterError, Kind, Mapper, Node, OriginIdx, Pass, RefIdx, Visitor};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

// FIXME: Move into utils crate?
#[doc(hidden)]
trait VecExt<T> {
    fn with(self, elt: T) -> Self;
}

impl<T> VecExt<T> for Vec<T> {
    fn with(mut self, elt: T) -> Vec<T> {
        self.push(elt);

        self
    }
}

/// Error reported when an item (variable, function, type) was already declared
/// in the current scope.
struct UniqueError(OriginIdx, &'static str);

/// A scope contains a set of available variables, functions and types.
#[derive(Clone, Default)]
struct Scope {
    variables: HashMap<Symbol, OriginIdx>,
    functions: HashMap<Symbol, OriginIdx>,
    types: HashMap<Symbol, OriginIdx>,
}

/// A scope map keeps track of the currently available scopes and the current depth
/// level.
#[derive(Clone, Default)]
struct ScopeMap {
    scopes: Vec<Scope>,
}

impl ScopeMap {
    fn get(
        &self,
        key: &Symbol,
        scope: usize,
        map_extractor: impl Fn(&Scope) -> &HashMap<Symbol, OriginIdx>,
    ) -> Option<&OriginIdx> {
        self.scopes
            .get(scope)
            .map(|_| &self.scopes[0..scope])
            .and_then(|scopes| {
                scopes
                    .iter()
                    .map(|scope| map_extractor(scope).get(key))
                    .find(|value| value.is_some())?
            })
    }

    fn insert_unique(
        &mut self,
        key: Symbol,
        value: OriginIdx,
        scope: usize,
        map_extractor: impl Fn(&mut Scope) -> &mut HashMap<Symbol, OriginIdx>,
    ) -> Result<(), OriginIdx> {
        let scope = match self.scopes.get_mut(scope) {
            Some(scope) => scope,
            None => {
                (self.scopes.len()..=scope)
                    .into_iter()
                    .for_each(|_| self.scopes.push(Scope::default()));

                &mut self.scopes[scope]
            }
        };

        let map = map_extractor(scope);

        match map.get(&key) {
            Some(existing) => Err(*existing),
            None => {
                map.insert(key, value);
                Ok(())
            }
        }
    }

    /// Maybe get a variable in any available scopes
    fn get_variable(&self, name: &Symbol, scope: usize) -> Option<&OriginIdx> {
        self.get(name, scope, |scope| &scope.variables)
    }

    /// Maybe get a function in any available scopes
    fn get_function(&self, name: &Symbol, scope: usize) -> Option<&OriginIdx> {
        self.get(name, scope, |scope| &scope.functions)
    }

    /// Maybe get a type in any available scopes
    fn get_type(&self, name: &Symbol, scope: usize) -> Option<&OriginIdx> {
        self.get(name, scope, |scope| &scope.types)
    }

    // FIXME: These are good but we need a way to return the OriginIdx to then emit the proper location and name
    /// Add a variable to the current scope if it hasn't been added before
    fn add_variable(
        &mut self,
        name: Symbol,
        scope: usize,
        var: OriginIdx,
    ) -> Result<(), UniqueError> {
        self.insert_unique(name, var, scope, |scope| &mut scope.variables)
            .map_err(|existing| UniqueError(existing, "variable"))
    }

    /// Add a function to the current scope if it hasn't been added before
    fn add_function(
        &mut self,
        name: Symbol,
        scope: usize,
        func: OriginIdx,
    ) -> Result<(), UniqueError> {
        self.insert_unique(name, func, scope, |scope| &mut scope.functions)
            .map_err(|existing| UniqueError(existing, "function"))
    }

    /// Add a type to the current scope if it hasn't been added before
    fn add_type(
        &mut self,
        name: Symbol,
        scope: usize,
        custom_type: OriginIdx,
    ) -> Result<(), UniqueError> {
        self.insert_unique(name, custom_type, scope, |scope| &mut scope.types)
            .map_err(|existing| UniqueError(existing, "type"))
    }
}

#[derive(Default)]
struct NameResolveCtx {
    current: OriginIdx,
    mappings: ScopeMap,
}

/// Extension type of [`Error`] to be able to implement [`IterError`].
struct NameResolutionError(Error);

enum UnresolvedKind {
    Call,
    Type,
    Var,
}

impl Display for UnresolvedKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> FmtResult {
        match self {
            UnresolvedKind::Call => write!(f, "call"),
            UnresolvedKind::Type => write!(f, "type"),
            UnresolvedKind::Var => write!(f, "var"),
        }
    }
}

impl NameResolutionError {
    /// Compose a [`UniqueError`] into a proper [`Error`] of kind [`ErrKind::NameResolution`]
    fn non_unique(
        fir: &Fir<FlattenData>,
        offending_loc: &Option<SpanTuple>,
        UniqueError(origin, kind_str): UniqueError,
    ) -> NameResolutionError {
        let existing = &fir.nodes[&origin];
        let sym = existing.data.symbol.as_ref().unwrap();

        NameResolutionError(
            Error::new(ErrKind::NameResolution)
                .with_msg(format!("{kind_str} `{sym}` already defined in this scope"))
                .with_loc(offending_loc.clone())
                .with_hint(
                    Error::hint()
                        .with_msg(format!("`{sym}` is also defined here"))
                        .with_loc(existing.data.location.clone()),
                ),
        )
    }

    fn unresolved(
        kind: UnresolvedKind,
        _mappings: &ScopeMap,
        sym: &Option<Symbol>,
        location: &Option<SpanTuple>,
    ) -> NameResolutionError {
        let sym = sym.as_ref().unwrap();

        // Extract possible values based on `kind` and `sym`

        // let map = match kind {
        //     UnresolvedKind::Call => todo!(),
        //     UnresolvedKind::Type => todo!(),
        //     UnresolvedKind::Var => todo!(),
        // }

        // let hints = self.mappings.keys().filter_map(|key| {
        //     if distance::levenshtein(key.symbol_unchecked().access(), err_sym.access()) < 2 {
        //         Some(
        //             Error::hint()
        //                 .with_msg(format!(
        //                     "maybe you meant to call `{}` instead?",
        //                     key.symbol_unchecked()
        //                 ))
        //                 .with_loc(key.location().clone()),
        //         )
        //     } else {
        //         None
        //     }
        // });

        NameResolutionError(
            Error::new(ErrKind::NameResolution)
                .with_msg(format!("unresolved {kind}: `{sym}`"))
                .with_loc(location.clone()),
        )
    }
}

impl IterError for NameResolutionError {
    fn simple() -> Self {
        NameResolutionError(Error::new(ErrKind::NameResolution))
    }

    fn aggregate(errs: Vec<Self>) -> Self {
        NameResolutionError(Error::new(ErrKind::Multiple(
            errs.into_iter().map(|e| e.0).collect(),
        )))
    }
}

struct Declarator<'ctx>(&'ctx mut NameResolveCtx);

impl<'ctx> Visitor<FlattenData, NameResolutionError> for Declarator<'ctx> {
    fn visit_function(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        _return_ty: &Option<RefIdx>,
        _block: &Option<RefIdx>,
    ) -> Fallible<NameResolutionError> {
        self.0
            .mappings
            .add_function(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| NameResolutionError::non_unique(fir, &node.data.location, ue))
    }

    fn visit_type(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _: &[RefIdx],
        _: &[RefIdx],
    ) -> Fallible<NameResolutionError> {
        self.0
            .mappings
            .add_type(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| NameResolutionError::non_unique(fir, &node.data.location, ue))
    }

    fn visit_instantiation(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _to: &RefIdx,
        _generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<NameResolutionError> {
        self.0
            .mappings
            .add_variable(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| NameResolutionError::non_unique(fir, &node.data.location, ue))
    }
}

struct Resolver<'ctx>(&'ctx mut NameResolveCtx);

impl<'ctx> Mapper<FlattenData, FlattenData, NameResolutionError> for Resolver<'ctx> {
    fn map_call(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        _to: RefIdx,
        generics: Vec<RefIdx>,
        args: Vec<RefIdx>,
    ) -> Result<Node<FlattenData>, NameResolutionError> {
        // FIXME: Is it fine to unwrap here?
        let definition = self
            .0
            .mappings
            .get_function(data.symbol.as_ref().unwrap(), data.scope)
            .map_or_else(
                || {
                    Err(NameResolutionError::unresolved(
                        UnresolvedKind::Call,
                        &self.0.mappings,
                        &data.symbol,
                        &data.location,
                    ))
                },
                |def| Ok(*def),
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

    fn map_typed_value(
        &mut self,
        data: FlattenData,
        origin: OriginIdx,
        value: RefIdx,
        ty: RefIdx,
    ) -> Result<Node<FlattenData>, NameResolutionError> {
        // nothing to do if there's no symbol
        if data.symbol.is_none() {
            return Ok(Node {
                data,
                origin,
                kind: Kind::TypedValue { value, ty },
            });
        }

        let definition = self
            .0
            .mappings
            // Unwrapping is okay here but ugly
            .get_variable(data.symbol.as_ref().unwrap(), data.scope)
            .map_or_else(
                || {
                    Err(NameResolutionError::unresolved(
                        UnresolvedKind::Var,
                        &self.0.mappings,
                        &data.symbol,
                        &data.location,
                    ))
                },
                |def| Ok(*def),
            )?;

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
        let definition = self
            .0
            .mappings
            // Unwrapping is okay here but ugly
            .get_type(data.symbol.as_ref().unwrap(), data.scope)
            .map_or_else(
                || {
                    Err(NameResolutionError::unresolved(
                        UnresolvedKind::Type,
                        &self.0.mappings,
                        &data.symbol,
                        &data.location,
                    ))
                },
                |def| Ok(*def),
            )?;

        Ok(Node {
            data,
            origin,
            kind: Kind::TypeReference(RefIdx::Resolved(definition)),
        })
    }
}

impl NameResolveCtx {
    fn insert_definitions(&mut self, fir: &Fir<FlattenData>) -> Fallible<NameResolutionError> {
        Declarator(self).visit(fir)
    }

    fn resolve_nodes(
        &mut self,
        fir: Fir<FlattenData>,
    ) -> Result<Fir<FlattenData>, NameResolutionError> {
        let mut resolver = Resolver(self);

        resolver.map(fir)
    }
}

impl Pass<FlattenData, FlattenData, Error> for NameResolveCtx {
    fn next_origin(&mut self) -> OriginIdx {
        let old = self.current;
        self.current = self.current.next();

        old
    }

    fn pre_condition(_fir: &Fir<FlattenData>) {}

    fn post_condition(_fir: &Fir<FlattenData>) {}

    // This should return a result :<
    fn transform(&mut self, fir: Fir<FlattenData>) -> Result<Fir<FlattenData>, Error> {
        let definition = self.insert_definitions(&fir);
        let resolution = self.resolve_nodes(fir);

        match (definition, resolution) {
            (Ok(_), Ok(fir)) => Ok(fir),
            (Ok(_), Err(NameResolutionError(e))) => {
                e.emit();
                Err(e)
            }
            (Err(NameResolutionError(e)), Ok(_)) => {
                e.emit();
                Err(e)
            }
            (Err(NameResolutionError(e1)), Err(NameResolutionError(e2))) => {
                let multi_err = Error::new(ErrKind::Multiple(vec![e1, e2]));
                multi_err.emit();
                Err(multi_err)
            }
        }
    }
}

pub trait NameResolve {
    fn name_resolve(self) -> Result<Fir<FlattenData>, Error>;
}

impl NameResolve for Fir<FlattenData> {
    fn name_resolve(self) -> Result<Fir<FlattenData>, Error> {
        let mut ctx = NameResolveCtx::default();

        ctx.pass(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore = "name resolution is not implemented yet (#593)"]
    fn name_resolve_simple() {
        let fir = Fir::<FlattenData> {
            nodes: HashMap::new(),
        }
        // we declare a function "a"
        .append(Node {
            data: FlattenData {
                symbol: Some(Symbol::from("a")),
                location: None,
                scope: 0,
            },
            origin: OriginIdx(0),
            kind: Kind::Function {
                generics: vec![],
                args: vec![],
                return_type: None,
                block: None,
            },
        })
        // we add a call to "a"
        .append(Node {
            data: FlattenData {
                symbol: Some(Symbol::from("a")),
                location: None,
                scope: 0,
            },
            origin: OriginIdx(1),
            kind: Kind::Call {
                to: RefIdx::Unresolved,
                generics: vec![],
                args: vec![],
            },
        });

        let fir = fir.name_resolve().unwrap();

        let call = &fir.nodes[&OriginIdx(1)];

        if let Kind::Call { to, .. } = call.kind {
            assert_eq!(to, RefIdx::Resolved(OriginIdx(0)))
        } else {
            panic!()
        }
    }
}
