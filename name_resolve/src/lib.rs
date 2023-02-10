use std::collections::HashMap;

use error::{ErrKind, Error};
use fir::{Fallible, Fir, IterError, Kind, Node, OriginIdx, Pass, RefIdx, Visitor};
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

// Compose a [`UniqueError`] into a proper [`Error`] of kind [`ErrKind::NameResolution`]
fn unique_error_to_def_error(
    fir: &Fir<FlattenData>,
    offending_loc: &Option<SpanTuple>,
    UniqueError(origin, kind_str): UniqueError,
) -> DefError {
    let existing = &fir.nodes[&origin];
    let sym = existing.data.symbol.as_ref().unwrap();

    DefError(
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

#[derive(Default)]
struct NameResolveCtx {
    current: OriginIdx,
    mappings: ScopeMap,
}

/// Extension type of [`Error`] to be able to implement [`IterError`].
struct DefError(Error);

struct Declarator<'ctx>(&'ctx mut NameResolveCtx);

impl IterError for DefError {
    fn simple() -> Self {
        DefError(Error::new(ErrKind::NameResolution))
    }

    fn aggregate(errs: Vec<Self>) -> Self {
        DefError(Error::new(ErrKind::Multiple(
            errs.into_iter().map(|e| e.0).collect(),
        )))
    }
}

impl<'ctx> Visitor<FlattenData, DefError> for Declarator<'ctx> {
    fn visit_function(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _generics: &[RefIdx],
        _args: &[RefIdx],
        _return_ty: &Option<RefIdx>,
        _block: &Option<RefIdx>,
    ) -> Fallible<DefError> {
        self.0
            .mappings
            .add_function(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| unique_error_to_def_error(fir, &node.data.location, ue))
    }

    fn visit_type(
        &mut self,
        fir: &Fir<FlattenData>,
        node: &Node<FlattenData>,
        _: &[RefIdx],
        _: &[RefIdx],
    ) -> Fallible<DefError> {
        self.0
            .mappings
            .add_type(
                node.data.symbol.as_ref().unwrap().clone(),
                node.data.scope,
                node.origin,
            )
            .map_err(|ue| unique_error_to_def_error(fir, &node.data.location, ue))
    }
}

struct Resolver<'ctx>(&'ctx mut NameResolveCtx);

impl<'ctx> Visitor<FlattenData, DefError> for Resolver<'ctx> {}

impl NameResolveCtx {
    fn insert_definitions(&mut self, fir: &Fir<FlattenData>) -> Fallible<DefError> {
        Declarator(self).visit(fir)
    }

    // FIXME: Should this return the Kind directly?
    fn resolve_node(&self, sym: &Symbol, scope: usize, kind: &Kind) -> RefIdx {
        match kind {
            Kind::Call { .. } => self
                .mappings
                .get_function(sym, scope)
                .map_or(RefIdx::Unresolved, |origin| RefIdx::Resolved(*origin)),
            // FIXME: Is that the correct node?
            Kind::TypeReference { .. } => self
                .mappings
                .get_type(sym, scope)
                .map_or(RefIdx::Unresolved, |origin| RefIdx::Resolved(*origin)),
            // FIXME: Is that the correct node?
            Kind::TypedValue { .. } => self
                .mappings
                .get_variable(sym, scope)
                .map_or(RefIdx::Unresolved, |origin| RefIdx::Resolved(*origin)),
            _ => RefIdx::Unresolved,
        }
    }

    fn resolve_nodes(&mut self, fir: Fir<FlattenData>) -> Fir<FlattenData> {
        fir.nodes
            .into_iter()
            .fold(Fir::default(), |fir, (origin, node)| {
                let kind = match node.kind {
                    Kind::Call { .. } | Kind::TypeReference(_) => {
                        let resolved = &node
                            .data
                            .symbol
                            .as_ref()
                            .map(|sym| self.resolve_node(sym, node.data.scope, &node.kind))
                            .unwrap();

                        match node.kind {
                            Kind::Call { generics, args, .. } => Kind::Call {
                                to: *resolved,
                                generics,
                                args,
                            },
                            // nothing to do for other types of nodes
                            kind => kind,
                        }
                    }
                    kind => kind,
                };

                fir.append(Node {
                    data: node.data,
                    origin,
                    kind,
                })
            })
    }

    fn error(&self, kind_str: &str, location: &Option<SpanTuple>, err_sym: &Symbol) -> Error {
        let hints = vec![].into_iter(); // FIXME: Remove

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

        let err = Error::new(ErrKind::NameResolution)
            .with_loc(location.clone())
            .with_msg(format!("unresolved {kind_str}: `{err_sym}`"));

        hints.fold(err, |e, hint| e.with_hint(hint))
    }

    fn check_for_unresolved(&self, fir: &Fir<FlattenData>) -> Result<(), Error> {
        let errs = fir
            .nodes
            .iter()
            .fold(Vec::<Error>::new(), |errs, (_, node)| match &node.kind {
                Kind::Call { to, .. } => {
                    if *to == RefIdx::Unresolved {
                        errs.with(self.error(
                            "function call",
                            &node.data.location,
                            node.data.symbol.as_ref().unwrap(),
                        ))
                    } else {
                        errs
                    }
                }
                _ => errs,
            });

        if errs.is_empty() {
            Ok(())
        } else {
            Err(Error::new(ErrKind::Multiple(errs)))
        }
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
        // FIXME: Is that pipeline correct? seems weird and annoying
        let definition = self.insert_definitions(&fir);

        let fir = self.resolve_nodes(fir);

        // TODO: can we do that in resolve_nodes?
        let usage = self.check_for_unresolved(&fir);

        match (definition, usage) {
            (Ok(_), Ok(_)) => Ok(fir),
            (Ok(_), Err(e)) => {
                e.emit();
                Err(e)
            }
            (Err(e), Ok(_)) => {
                e.0.emit();
                Err(e.0)
            }
            (Err(e1), Err(e2)) => {
                let multi_err = Error::new(ErrKind::Multiple(vec![e1.0, e2]));
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
