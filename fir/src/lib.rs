//! FIR stands for Flat Intermediate Representation

// FIXME:
// How do we represent types here? How would we perform type checking? How do we perform name resolution?
// Do we keep references? IDs?
// How do we implement our "contracts"? What would they look like here? This will probably end up being super
// jinko-specific
// Can we do something like only keep an ImmutableMap here (maybe more helpers but that's it) and then only
// have extension types which help rebuild a new Fir graph?
// And have a *lot* of enum variants which are then checked for existence during the various passes? i.e
// have `UnresolvedCall` and `UnresolvedVar` as variants, and then after name resolution make sure that
// they are not present anymore?
// fir.nodes.iter().for_each(|node| {
//     match node {
//         UnresolvedCall | UnresolvedVar => unreachable!(),
//         _ => {}
//     }
// })
//
// The base kind would thus be the equivalent of a "leaf" node - something which does not refer to any other nodes,
// and is only referred to. So in the case of jinko we can think of it as Types probably?
// Each variable, each function will refer to a type - that is typechecking.
// functions also refer to types after typechecking. So are the basic nodes types? Constants? These go together, and
// should maybe be grouped as "Values"? Where do we store them? How?
//
// We need trait extensions. This way the name resolver can keep its own mappings and update the Fir (-> return a new one)
// And for example the VM or FIR-e can keep some execution context as it runs through the VM.
// So the entry point of the trait extension would be, e.g `name_resolve` which would create a [`NameResolver`] ctx
// and visit the entire Fir, finally returning a complete one.
//
// How do represent this internally within the [`Kind`] enum? Do we keep something like
// enum Kind {
//     Declaration(...),
//     UnresolvedCall(Symbol, Vec<Node>, Vec<Node>),
//     ResolvedCall(FirIdx, Vec<FirIdx>, Vec<FirIdx>),
// }
// Should name resolution be done separately and Fir only works using FirIdx? Yes?
// Can we have that and have the typechecker work? By simply changing relations in the graph?
// Does that make sense? Does that indicate that for all types we must first keep a Option<Ty> which is set to None?
// Is this going to cause problems?

use std::collections::HashMap;
use std::hash::Hash;

use location::SpanTuple;
use symbol::Symbol;

type FirIdx = u64;

pub trait WithHashMap<K: Hash + Eq, V> {
    fn with(self, key: K, value: V) -> Self;
}

impl<K: Hash + Eq, V> WithHashMap<K, V> for HashMap<K, V> {
    fn with(mut self, key: K, value: V) -> HashMap<K, V> {
        self.insert(key, value);

        self
    }
}

#[derive(Debug, Clone)]
pub enum Kind {
    Declaration(Symbol),
    UnresolvedCall {
        to: Symbol,
        generics: Vec<Node>,
        args: Vec<Node>,
    },
    Call {
        to: FirIdx,
        generics: Vec<FirIdx>,
        args: Vec<FirIdx>,
    },
}

#[derive(Debug, Clone)]
pub struct Node {
    pub location: Option<SpanTuple>,
    pub kind: Kind,
}

/// An instance of [`Fir`] is similar to a graph, containing [`Node`]s and relationships binding them together.
#[derive(Default)]
pub struct Fir {
    pub nodes: HashMap<FirIdx, Node>,
}

struct NameResolver {
    mappings: HashMap<String, FirIdx>,
}

fn name_resolve(fir: Fir) -> Fir {
    // How do we have a visitor using this baby?
    let resolver = NameResolver {
        mappings: HashMap::new(),
    };

    fir.nodes.into_iter().fold(Fir::default(), |fir, node| {
        let (_, value) = node;
        let Node { kind, location } = value;
        let resolved = match kind {
            Kind::UnresolvedCall {
                to: _,
                generics: _,
                args: _,
            } => {
                // how do we get an idx from the `to` here, which is a symbol?
                // how do we visit each generic node?
                // how do we visit each argument node?
                let _idx = fir.nodes.get(&15);
                // we need visitors here to resolve generics and arguments

                Node {
                    location,
                    kind: Kind::Call {
                        to: 15,
                        generics: vec![],
                        args: vec![],
                    },
                }
            }
            // Forbidden nodes
            Kind::Call { .. } => unreachable!(),
            // Nothing to do for the other nodes
            _ => todo!(),
        };

        Fir {
            nodes: fir.nodes.with(16, resolved),
        }
    })
}

// pub fn insert_call(fir: Fir) -> Fir {
//     let fn_idx = fir.functions.get(&15).unwrap();
//     let call = Node {
//         location: None,
//         kind: Kind::Call(*fn_idx),
//     };

//     Fir {
//         nodes: fir.nodes.with(16, call),
//         ..fir
//     }
// }
