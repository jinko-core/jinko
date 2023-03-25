use std::collections::HashMap;

use error::{ErrKind, Error};
use fir::{Fallible, Fir, Incomplete, Mapper, OriginIdx, Pass, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

mod declarator;
mod resolver;

use declarator::Declarator;
use resolver::{ResolveKind, Resolver};

/// Error reported when an item (variable, function, type) was already declared
/// in the current scope.
struct UniqueError(OriginIdx, &'static str);

/// A scope contains a set of available variables, functions and types.
#[derive(Clone, Default, Debug)]
struct Scope {
    variables: HashMap<Symbol, OriginIdx>,
    functions: HashMap<Symbol, OriginIdx>,
    types: HashMap<Symbol, OriginIdx>,
}

/// A scope map keeps track of the currently available scopes and the current depth
/// level.
#[derive(Clone, Default, Debug)]
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
            // FIXME: This is buggy if there aren't any scopes, so if we get without having inserted first. e.g with the following code
            // ```jinko
            // name = "jinko"; // no declaration, just a binding, so we "get" without having created the scope first
            // ```
            .map_or(
                // This is a workaround for now but Wow! it's super fucking ugly
                match scope {
                    1 => None,
                    _ => Some(self.scopes.len() - 1),
                },
                |_| Some(scope),
            )
            .map(|last| &self.scopes[0..=last])
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
                (self.scopes.len()..=scope).for_each(|_| self.scopes.push(Scope::default()));

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

    /// Add a variable to the current scope if it hasn't been added before
    fn add_variable(
        &mut self,
        name: Symbol,
        scope: usize,
        var: OriginIdx,
    ) -> Result<(), UniqueError> {
        self.insert_unique(name, var, scope, |scope| &mut scope.variables)
            .map_err(|existing| UniqueError(existing, "binding"))
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
    mappings: ScopeMap,
}

/// Extension type of [`Error`] to be able to implement [`IterError`].
enum NameResolutionError {
    NonUnique(SpanTuple, OriginIdx, &'static str),
    Unresolved(ResolveKind, Symbol, SpanTuple),
    AmbiguousBinding(OriginIdx, OriginIdx, SpanTuple),
    UnresolvedBinding(Symbol, SpanTuple),
    Multiple(Vec<NameResolutionError>),
}

impl NameResolutionError {
    fn finalize(self, fir: &Fir<FlattenData>, _mappings: &ScopeMap) -> Error {
        // we need the FIR... how do we access nodes otherwise??
        match self {
            NameResolutionError::Multiple(errs) => Error::new(ErrKind::Multiple(
                errs.into_iter()
                    .map(|e| e.finalize(fir, _mappings))
                    .collect(),
            )),
            NameResolutionError::NonUnique(loc, origin, kind_str) => {
                let existing = &fir.nodes[&origin].data;
                let sym = existing.ast.symbol().unwrap();

                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("{kind_str} `{sym}` already defined in this scope"))
                    .with_loc(loc)
                    .with_hint(
                        Error::hint()
                            .with_msg(format!("`{sym}` is also defined here"))
                            .with_loc(existing.ast.location().clone()),
                    )
            }
            NameResolutionError::Unresolved(kind, sym, location) => {
                // TODO: Extract possible values based on `kind` and `sym`

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
                //                     "maybe you meant `{}`?",
                //                     key.symbol_unchecked()
                //                 ))
                //                 .with_loc(key.location().clone()),
                //         )
                //     } else {
                //         None
                //     }
                // });

                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("unresolved {kind}: `{sym}`"))
                    .with_loc(location)
            }
            NameResolutionError::AmbiguousBinding(lhs, rhs, location) => {
                let lhs = &fir.nodes[&lhs].data;
                let rhs = &fir.nodes[&rhs].data;
                let sym = lhs.ast.symbol().unwrap();

                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("resolution of `{sym}` is ambiguous"))
                    .with_loc(location)
                    .with_hint(
                        Error::hint()
                            .with_msg(String::from("could point to this binding..."))
                            .with_loc(lhs.ast.location().clone()),
                    )
                    .with_hint(
                        Error::hint()
                            .with_msg(String::from("...or this empty type"))
                            .with_loc(rhs.ast.location().clone()),
                    )
            }
            NameResolutionError::UnresolvedBinding(sym, location) => {
                // TODO: Go through mappings again to find a relevant type or var which could work

                Error::new(ErrKind::NameResolution)
                    .with_msg(format!("unresolved binding to {sym}"))
                    .with_loc(location)
                    .with_hint(
                        Error::hint().with_msg(format!("searched for empty type named {sym}")),
                    )
                    .with_hint(Error::hint().with_msg(format!("searched for binding named {sym}")))
            }
        }
    }

    /// Compose a [`UniqueError`] into a proper [`Error`] of kind [`ErrKind::NameResolution`]
    fn non_unique(
        location: &SpanTuple,
        UniqueError(origin, kind_str): UniqueError,
    ) -> NameResolutionError {
        NameResolutionError::NonUnique(location.clone(), origin, kind_str)
    }

    fn unresolved(
        kind: ResolveKind,
        sym: Option<&Symbol>,
        location: &SpanTuple,
    ) -> NameResolutionError {
        NameResolutionError::Unresolved(kind, sym.cloned().unwrap(), location.clone())
    }

    fn ambiguous_binding(
        lhs: OriginIdx,
        rhs: OriginIdx,
        location: &SpanTuple,
    ) -> NameResolutionError {
        NameResolutionError::AmbiguousBinding(lhs, rhs, location.clone())
    }

    fn unresolved_binding(sym: Option<&Symbol>, location: &SpanTuple) -> NameResolutionError {
        NameResolutionError::UnresolvedBinding(sym.cloned().unwrap(), location.clone())
    }
}

impl NameResolveCtx {
    fn insert_definitions(&mut self, fir: &Fir<FlattenData>) -> Fallible<Vec<NameResolutionError>> {
        Declarator(self).traverse(fir)
    }

    fn resolve_nodes<'ast>(
        &mut self,
        fir: Fir<FlattenData<'ast>>,
    ) -> Result<Fir<FlattenData<'ast>>, Incomplete<FlattenData<'ast>, NameResolutionError>> {
        Resolver(self).map(fir)
    }
}

impl<'ast> Pass<FlattenData<'ast>, FlattenData<'ast>, Error> for NameResolveCtx {
    fn pre_condition(_fir: &Fir<FlattenData>) {}

    fn post_condition(_fir: &Fir<FlattenData>) {}

    fn transform(&mut self, fir: Fir<FlattenData<'ast>>) -> Result<Fir<FlattenData<'ast>>, Error> {
        let definition = self.insert_definitions(&fir);
        let definition = definition
            .map_err(|errs| NameResolutionError::Multiple(errs).finalize(&fir, &self.mappings));

        let resolution = self.resolve_nodes(fir);

        match (definition, resolution) {
            (Ok(_), Ok(fir)) => Ok(fir),
            (Ok(_), Err(Incomplete { carcass, errs })) => {
                Err(NameResolutionError::Multiple(errs).finalize(&carcass, &self.mappings))
            }
            (Err(e), Ok(_fir)) => Err(e),
            (Err(e1), Err(Incomplete { carcass, errs })) => {
                let multi_err = Error::new(ErrKind::Multiple(vec![
                    e1,
                    NameResolutionError::Multiple(errs).finalize(&carcass, &self.mappings),
                ]));
                Err(multi_err)
            }
        }
    }
}

pub trait NameResolve<'ast> {
    fn name_resolve(self) -> Result<Fir<FlattenData<'ast>>, Error>;
}

impl<'ast> NameResolve<'ast> for Fir<FlattenData<'ast>> {
    fn name_resolve(self) -> Result<Fir<FlattenData<'ast>>, Error> {
        let mut ctx = NameResolveCtx::default();

        ctx.pass(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fir::{Kind, RefIdx};
    use flatten::FlattenAst;
    use xparser::ast;

    #[test]
    fn declaration() {
        let ast = ast! {
            where a = 15;
            where b = a;
        };

        let fir = ast.flatten().name_resolve().unwrap();

        let a = &fir.nodes[&OriginIdx(2)];
        let a_reference = &fir.nodes[&OriginIdx(3)];

        assert!(matches!(a_reference.kind, Kind::TypedValue { .. }));
        let a_reference = match a_reference.kind {
            Kind::TypedValue { value, .. } => value,
            _ => unreachable!(),
        };

        assert_eq!(a_reference, RefIdx::Resolved(a.origin));
    }

    #[test]
    fn function_call() {
        let ast = ast! {
            func a() {}

            a();
        };

        let fir = ast.flatten().name_resolve().unwrap();

        // find the unique call and definition
        let def = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::Function { .. }));
        let call = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::Call { .. }))
            .map(|node| match node.kind {
                Kind::Call { to, .. } => to,
                _ => unreachable!(),
            });

        assert_eq!(call.unwrap(), RefIdx::Resolved(def.unwrap().origin));
    }

    #[test]
    fn type_def() {
        let ast = ast! {
            type T;

            where x = T;
        };

        let fir = ast.flatten().name_resolve().unwrap();

        // find the unique call and definition
        let def = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::Type { .. }));
        let (value, ty) = fir
            .nodes
            .values()
            .find(|node| matches!(node.kind, Kind::TypedValue { .. }))
            .map(|node| match node.kind {
                Kind::TypedValue { value, ty } => (value, ty),
                _ => unreachable!(),
            })
            .unwrap();

        assert_eq!(value, RefIdx::Resolved(def.unwrap().origin));
        assert_eq!(ty, RefIdx::Resolved(def.unwrap().origin));
    }

    #[test]
    fn ambiguous_var() {
        let ast = ast! {
            where x = X;
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_err())
    }

    #[test]
    fn function_argument() {
        let ast = ast! {
            type int;

            func id(x: int) -> int { x }
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok());
    }

    #[test]
    fn complex() {
        let ast = ast! {
            type int;

            func id(x: int) -> int {
                x
            }

            type Id(value: int);

            where x = Id(value: 15);
            where y = 14;
            where z = id(y);
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_ok());
    }

    #[test]
    fn builtin_type() {
        let ast = ast! {
            type bool;
            type true;
            type false;

            func foo() -> bool { true }
        };

        let fir = ast.flatten().name_resolve();

        if let Err(e) = &fir {
            e.emit();
        }

        assert!(fir.is_ok());
    }

    #[test]
    fn scoped_resolution() {
        let ast = ast! {
            type Marker;

            {
                type Marker;
                where x = Marker;
            }
        };

        let fir = ast.flatten().name_resolve().unwrap();

        let x_value = &fir.nodes[&OriginIdx(3)];
        let marker_1 = &fir.nodes[&OriginIdx(2)];
        let marker_2 = &fir.nodes[&OriginIdx(1)];

        assert!(matches!(x_value.kind, Kind::TypedValue { .. }));
        assert!(matches!(marker_1.kind, Kind::Type { .. }));
        assert!(matches!(marker_2.kind, Kind::Type { .. }));

        match x_value.kind {
            Kind::TypedValue { value, ty } => {
                assert_eq!(value, ty);
                assert_eq!(ty, RefIdx::Resolved(marker_2.origin));
                assert_eq!(value, RefIdx::Resolved(marker_2.origin));
            }
            _ => unreachable!(),
        }
    }
}
