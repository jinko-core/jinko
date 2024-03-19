// How do we want this to work? Something like collect all the things that need to be monomorphized, mono them and then recreate the links in the FIR?
// E.g if we have
//
// ```rust
// func foo[T](a: T) -> T { a.bar() }
//
// foo(15);
// ```
//
// `foo(15)` will be a generic call, so we'll mono the function we're calling into - but then we need to mono each of its statements
// (this is similar for types etc)
// so we want our Mono call to return the new OriginIdx we'll be resolved to
//
// e.g. `foo(15)` currently resolves to `foo[T]`, so we'll mono and create `foo[int]` which will return a new OriginIdx
// then, we want `foo(15)` to instead resolve to `foo[int]`
// this is the same thing for all the statements - currently `foo[T]` statements are [ `a.bar()` ], which is the un-mono'd version where `a` is still `T`.
// we need to mono that and get a list of mono'd statements back, which we'll assign to `foo[int]` - this is tree-like.
// should we have a tree-like mapper? how would that even work?
// how do we do that properly within the current FIR system?
//
// so this should probably be split into two visitors and one generator
//
// 1. Collect what needs to be mono'd. Create a Map with the expected new OriginIdx, thanks to a counter updated in the visitor
//    GenericCollector(Map<OriginIdx, (Substitutions, NewOriginIdx)>). This is done through a TreeLike visitor because the subs
//    are available at the definition level (type/function) but need to be used for all the children (fields/statements)
//    so this is how we collect them. Each child gets its entry in the map and a copy of the substitutions to perform
//
// 2. Mono all required nodes, basically doing copy paste. does this need to be a Mapper/multimapper? How do we mono based on the kind?
//    Something like (if to_mono.contains(this) -> mono, return vec![this, mono(this)]
//    this would work and be quite simple!
//
// 3. Finally, replace all references to their mono'd equivalent - so this is just a mapper, looking at the current ctx and doing a replace when necessary
//    if (to_mono.contains(this) { this.resolved = to_mono.get(this) });
//
// (1) should be used alongside the constraint builder, which builds constraints for generic functions, but that can be done later?
// (2) is a simple multimapper we can do here, and (3) is a super simple mapper which is going to be just a couple functions for function calls and type instantiations. name it MonoReplace?
// TODO(Arthur): How to name (1)? SubstitutionBuilder?
//
// wait, is this actually going to work? when we create the substitution builder, we need to recreate tree-like structures in the map, so that nodes are mono'd properly
// e.g. we can't just copy over the fields of a type declaration, because then (3) would replace them each time since the FIR is one big linked list
// e.g. let's take a look at this:
// ```rust
// type Foo[T](a: T, b: T)
// ```
// this is represented as the following FIR:
//
// ```
// {
//     1: Field(a, T),
//     2: Field(b, T),
//     3: TypeDec(Foo, [T], [1, 2]),
// }
// ```
// if we mono on `int` and `string` and just copy over the fields, we'll have this:
//
// ```
// {
//     1: Field(a, T),
//     2: Field(b, T),
//     3: TypeDec(Foo, [T], [1, 2]),
//     4: TypeDec(Foo[int], [], [1, 2]),
//     5: TypeDec(Foo[string], [], [1, 2]), // note the same nodes being used
// }
// ```
// which will be an issue since our SubstitutionMap will contain something like { 1: Monod(1, int), 2: Monod(2, int) }
// or { 1: Monod(1, string), 2: Monod(2, string) }
// meaning that when we do perform the generation and replacement we'll have the following:
// ```
// {
//     1: Field(a, T),
//     2: Field(b, T),
//     3: TypeDec(Foo, [T], [1, 2]),
//     4: TypeDec(Foo[int], [], [1, 2]),
//     5: TypeDec(Foo[string], [], [1, 2]),
//     6: Field(a, string)
//     7: Field(b, string)
// }
// ```
// and
//
// ```
// {
//     1: Field(a, T),
//     2: Field(b, T),
//     3: TypeDec(Foo, [T], [1, 2]),
//     4: TypeDec(Foo[int], [], [6, 7]), // HERE: Wrong mono!
//     5: TypeDec(Foo[string], [], [6, 7]), // good mono
//     6: Field(a, string)
//     7: Field(b, string)
// }
// ```
// so we actually have to "allocate" (and by that I mean create new OriginIdx) before doing the generation and replacement
// so when building the subs map, we need to keep that context alive - and visit a definition point everytime we visit a call point!
// NOTE(Arthur): Important! basically our SubstitutionMap will be more something like `Map<OriginIdx, Vec<(Substitutions, NewOriginIdx)>`
// how does that affect our generator? something like multimapper where we take in Foo[T], a Map { Foo[T]: Foo[int], Foo[string] } and return vec![foo[int], foo[string]]?
// do we return the generic version? no, right?

use fir::{Fir, Incomplete, Kind, MultiMapper, Node, OriginIdx, RefIdx};
use flatten::FlattenData;

use super::substitutions;

pub struct Monomorphize {
    input: substitutions::Output,
}

#[derive(Debug)]
pub struct Error;

// FIXME: This can probably benefit from using a different `U` here - something like T -> U becomes FlattenData -> (MonodIdx, FlattenData)
// Also the `From: T` in Mapper should probably be a `From: (&Node, T)` - so that we get all the info possible
impl<'ast> MultiMapper<FlattenData<'ast>, FlattenData<'ast>, Error> for Monomorphize {
    // fn map_record_type(
    //     &mut self,
    //     _data: FlattenData<'ast>,
    //     origin: OriginIdx,
    //     generics: Vec<RefIdx>,
    //     fields: Vec<RefIdx>,
    // ) -> Result<Vec<Node<FlattenData<'ast>>>, Error> {
    //     self.input.to_mono.get(&origin).map(|request| {
    //       request
    //     })
    // }
}
