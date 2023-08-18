use std::{collections::HashMap, ops::Index};

use error::{ErrKind, Error};
use fir::{Fallible, Fir, Incomplete, Kind, Mapper, OriginIdx, Pass, Traversal};
use flatten::FlattenData;
use location::SpanTuple;
use symbol::Symbol;

mod declarator;
mod resolver;
mod scoper;

use declarator::Declarator;
use resolver::{ResolveKind, Resolver};
use scoper::Scoper;

/// Error reported when an item (variable, function, type) was already declared
/// in the current scope.
struct UniqueError(OriginIdx, &'static str);

/// Documentation: very useful data structure, useful for two things:
/// 1. knowing where nodes live
/// 2. knowing which scope is the parent scope of a given scope
/// Keeps a reference on a hashmap - cheap to copy and pass around
#[derive(Clone, Copy, Debug)]
struct EnclosingScope<'enclosing>(&'enclosing HashMap<OriginIdx, OriginIdx>);

impl Index<OriginIdx> for EnclosingScope<'_> {
    type Output = OriginIdx;

    fn index(&self, index: OriginIdx) -> &Self::Output {
        &self.0[&index]
    }
}

type Bindings = HashMap<Symbol, OriginIdx>;

// FIXME: Documentation
// Each scope contains a set of bindings
#[derive(Clone, Debug)]
struct FlatScope<'enclosing> {
    scopes: HashMap<OriginIdx, Bindings>,
    enclosing_scope: EnclosingScope<'enclosing>,
}

/// Allow iterating on a [`FlatScope`] by going through the chain of enclosing scopes
struct FlatIterator<'scope, 'enclosing>(Option<OriginIdx>, &'scope FlatScope<'enclosing>);

trait LookupIterator<'scope, 'enclosing> {
    fn lookup_iterator(&'scope self, starting_scope: OriginIdx)
        -> FlatIterator<'scope, 'enclosing>;
}

impl<'scope, 'enclosing> LookupIterator<'scope, 'enclosing> for FlatScope<'enclosing> {
    fn lookup_iterator(
        &'scope self,
        starting_scope: OriginIdx,
    ) -> FlatIterator<'scope, 'enclosing> {
        FlatIterator(Some(starting_scope), self)
    }
}

impl<'scope, 'enclosing> Iterator for FlatIterator<'scope, 'enclosing> {
    type Item = &'scope Bindings;

    fn next(&mut self) -> Option<Self::Item> {
        let cursor = self.0;
        let bindings = cursor.and_then(|scope_idx| self.1.scopes.get(&scope_idx));

        self.0 = cursor
            .and_then(|current| self.1.enclosing_scope.0.get(&current))
            .copied();

        bindings
    }
}

impl<'enclosing> FlatScope<'enclosing> {
    fn lookup(&self, name: &Symbol, starting_scope: OriginIdx) -> Option<&OriginIdx> {
        self.lookup_iterator(starting_scope)
            .find_map(|bindings| bindings.get(name))
    }

    fn insert(
        &mut self,
        name: Symbol,
        idx: OriginIdx,
        scope: OriginIdx, /* FIXME: Needs newtype idiom */
    ) -> Result<(), OriginIdx> {
        // we need to use the innermost scope here, not `lookup`
        if let Some(existing) = self
            .scopes
            .get(&scope)
            .expect("interpreter error: there should always be at least one outer scope")
            .get(&name)
        {
            return Err(*existing);
        }

        self.scopes
            .entry(scope)
            .or_insert_with(HashMap::new)
            .insert(name, idx);

        Ok(())
    }
}

/// A scope map keeps track of the currently available scopes and the current depth
/// level.
#[derive(Clone, Debug)]
struct ScopeMap<'enclosing> {
    pub variables: FlatScope<'enclosing>,
    pub functions: FlatScope<'enclosing>,
    pub types: FlatScope<'enclosing>,
}

struct NameResolveCtx<'enclosing> {
    enclosing_scope: EnclosingScope<'enclosing>,
    mappings: ScopeMap<'enclosing>,
}

impl<'enclosing> NameResolveCtx<'enclosing> {
    fn new(enclosing_scope: EnclosingScope<'enclosing>) -> NameResolveCtx {
        let empty_scope_map: HashMap<OriginIdx, Bindings> = enclosing_scope
            .0
            .values()
            .map(|scope_idx| (*scope_idx, Bindings::new()))
            .collect();

        NameResolveCtx {
            enclosing_scope,
            mappings: ScopeMap {
                variables: FlatScope {
                    scopes: empty_scope_map.clone(),
                    enclosing_scope,
                },
                functions: FlatScope {
                    scopes: empty_scope_map.clone(),
                    enclosing_scope,
                },
                types: FlatScope {
                    scopes: empty_scope_map,
                    enclosing_scope,
                },
            },
        }
    }
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

impl<'enclosing> NameResolveCtx<'enclosing> {
    fn scope(fir: &Fir<FlattenData>) -> HashMap<OriginIdx, OriginIdx> {
        let root = fir.nodes.last_key_value().unwrap();

        let mut scoper = Scoper {
            current_scope: *root.0,
            enclosing_scope: HashMap::new(),
        };

        let Kind::Statements(stmts) = &root.1.kind else { unreachable!() };

        stmts
            .iter()
            .for_each(|stmt| scoper.traverse_node(fir, &fir[stmt]).unwrap());

        scoper.enclosing_scope
    }

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

impl<'ast, 'enclosing> Pass<FlattenData<'ast>, FlattenData<'ast>, Error>
    for NameResolveCtx<'enclosing>
{
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
        // TODO: Ugly asf
        let enclosing_scope = NameResolveCtx::scope(&self);
        let mut ctx = NameResolveCtx::new(EnclosingScope(&enclosing_scope));

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
        let Kind::TypedValue { value: a_reference, .. } = a_reference.kind else { unreachable!() };

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

        assert!(fir.is_err());
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

        let marker_1 = dbg!(&fir.nodes[&OriginIdx(1)]);
        let marker_2 = dbg!(&fir.nodes[&OriginIdx(2)]);
        let x_value = dbg!(&fir.nodes[&OriginIdx(3)]);

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

    #[test]
    fn fail_resolution_to_fn_arg() {
        let ast = ast! {
            type A;

            func f(a: A) {}

            where x = a; // invalid
        };

        let fir = ast.flatten().name_resolve();

        assert!(fir.is_err());
    }
}
