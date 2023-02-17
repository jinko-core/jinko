use std::collections::HashMap;

use error::{ErrKind, Error};
use fir::{Fallible, Fir, IterError, Mapper, OriginIdx, Pass, Traversal};
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
            .map(|_| &self.scopes[0..=scope])
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
    mappings: ScopeMap,
}

/// Extension type of [`Error`] to be able to implement [`IterError`].
struct NameResolutionError(Error);

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
        kind: ResolveKind,
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

impl NameResolveCtx {
    fn insert_definitions(&mut self, fir: &Fir<FlattenData>) -> Fallible<NameResolutionError> {
        Declarator(self).visit(fir)
    }

    fn resolve_nodes(
        &mut self,
        fir: Fir<FlattenData>,
    ) -> Result<Fir<FlattenData>, NameResolutionError> {
        Resolver(self).map(fir)
    }
}

impl Pass<FlattenData, FlattenData, Error> for NameResolveCtx {
    // FIXME: This should be removed from Pass and added to MultiMapper
    fn next_origin(&mut self) -> OriginIdx {
        OriginIdx(0)
    }

    fn pre_condition(_fir: &Fir<FlattenData>) {}

    fn post_condition(_fir: &Fir<FlattenData>) {}

    // This should return a result :<
    fn transform(&mut self, fir: Fir<FlattenData>) -> Result<Fir<FlattenData>, Error> {
        let definition = self.insert_definitions(&fir);
        let resolution = self.resolve_nodes(fir);

        match (definition, resolution) {
            (Ok(_), Ok(fir)) => Ok(fir),
            (Ok(_), Err(NameResolutionError(e))) => Err(e),
            (Err(NameResolutionError(e)), Ok(_)) => Err(e),
            (Err(NameResolutionError(e1)), Err(NameResolutionError(e2))) => {
                let multi_err = Error::new(ErrKind::Multiple(vec![e1, e2]));
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
    use fir::{Kind, RefIdx};

    macro_rules! fir {
        ($($tok:tt)*) => {
            {
                let ast = xparser::parse(
                    stringify!($($tok)*),
                    location::Source::Input(stringify!($($tok)*)))
                .unwrap();

                flatten::FlattenAst::flatten(&ast)
            }
        }
    }

    #[test]
    fn declaration() {
        let fir = fir! {
            where a = 15;
            where b = a;
        }
        .name_resolve()
        .unwrap();

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
        let fir = fir! {
            func a() {}

            a();
        }
        .name_resolve()
        .unwrap();

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
}
