// them with.

// so we would like to traverse call points, and to build a map of declaration points with what to substitute them
// Find all the generic callpoints - find their associated declpoints - add that to the Map<Declpoint, Vec<(Subs, NewOriginIdx)>>
// TODO: How to handle children? We need to visit those as well and create a mono request too
// NOTE: We want to mono ALL children - even if they do not make use of the substitutions. Otherwise we'll probably run into issues, right?
// maybe we can revisit this later as a space optimization?

// e.g. let's look at the following code:
//
// ```text
// type Foo[T](a: T, b: T);
//
// where f = Foo[int](a: 15, b: 14);
// ```
// the FIR for this looks like the following:
//
// ```text
// {
//    1: Field(a, T)
//    2: Field(b, T)
//    3: Type(Foo, [T], [1, 2])
//    4: Assign(1, Constant(15))
//    5: Assign(2, Constant(14))
//    6: Instantiation(3 /* Foo */, [int], [4, 5])
// }
// ```
// NOTE: Do we need two maps? One for declpoints and one for callpoints? or is that not necessary and I'm going about it the wrong way
// the output we want is something like this:
// ```text
// { 1: Subs(T -> int), NewIdx(7) }
// { 2: Subs(T -> int), NewIdx(8) }
// { 3: Subs(T -> int), NewIdx(9) }
// ```
// then we'll have to figure out 1. how to do those substitutions 2. how to replace `3` by `9` (and others) in the FIR

use fir::iter::TreeLike;
use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use flatten::FlattenData;

use core::mem;
use std::collections::HashMap;
use std::marker::PhantomData;

pub struct NewOrigin(OriginIdx);

pub struct OriginVector(Vec<OriginIdx>);

impl From<&[RefIdx]> for OriginVector {
    fn from(references: &[RefIdx]) -> OriginVector {
        let origins = references.iter().map(|r| r.expect_resolved()).collect();

        OriginVector(origins)
    }
}

pub struct Generics<T> {
    generics: OriginVector,
    _marker: PhantomData<T>,
}

impl<T> Generics<T> {
    pub fn new(generics: impl Into<OriginVector>) -> Generics<T> {
        Generics {
            generics,
            _marker: PhantomData,
        }
    }
}

pub struct Decls(Generics<Decls>);
pub struct Args(Generics<Args>);

pub struct SubsTarget {
    from: Decls,
    to: Args,
}

impl SubsTarget {
    pub fn new(from: Decls, to: Args) -> SubsTarget {
        SubsTarget { from, to }
    }
}

pub type Output = HashMap<RefIdx, Vec<(SubsTarget, NewOrigin)>>;

pub struct Substitutions {
    output: Output,
    next_idx: OriginIdx,
}

impl Substitutions {
    // FIXME: Should this return Self?
    pub fn find<'ast>(fir: &Fir<FlattenData<'ast>>) -> Output {
        let next_idx = fir
            .nodes
            .last_key_value()
            .map(|kv| kv.0.next())
            .unwrap_or(OriginIdx(1));

        let mut ctx = Substitutions {
            output: Output::new(),
            next_idx,
        };

        ctx.visit_all(fir);

        ctx.output
    }

    pub fn next_origin(&mut self) -> NewOrigin {
        let new_next = self.next_idx.next();
        let next = mem::replace(&mut self.next_idx, new_next);

        NewOrigin(next)
    }

    pub fn add(&mut self, fir: &Fir<FlattenData<'_>>, to: &RefIdx, generics: &[RefIdx]) {
        let from = &fir[to];
        let from = match &from.kind {
            Kind::Function { generics, .. } => generics.as_slice(),
            _ => unreachable!(),
        };
        let new_sub = SubsTarget::new(Decls(Generics::new(from)), Args(Generics::new(generics)));
        // let new_v = || (new_sub, self.next_origin());

        // self.output
        //     .entry(*to)
        //     .and_modify(|mut v| v.push(new_v()))
        //     .or_insert_with(|| vec![new_v()]);
    }
}

impl<'ast> TreeLike<FlattenData<'ast>> for Substitutions {
    fn visit_call(
        &mut self,
        fir: &Fir<FlattenData<'ast>>,
        _node: &Node<FlattenData<'ast>>,
        to: &RefIdx,
        generics: &[RefIdx],
        args: &[RefIdx],
    ) {
        if !generics.is_empty() {
            self.add(fir, to, generics);

            self.visit_many(fir, args)
        }
    }
}
