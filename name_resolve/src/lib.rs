use std::collections::HashMap;

use fir::{Fir, Kind, Node, OriginIdx, Pass, RefIdx};
use symbol::Symbol;

struct NameResolveCtx {
    current: OriginIdx,
    mappings: HashMap<Symbol, OriginIdx>,
}

impl NameResolveCtx {
    fn insert_nodes(&mut self, fir: &Fir<Symbol>) {
        fir.nodes.iter().for_each(|kv| {
            let (origin, node) = kv;

            if let Kind::FnDeclaration { .. } = &node.kind {
                self.mappings.insert(node.data.clone(), *origin);
            }
        });
    }

    fn resolve_symbol(&self, sym: &Symbol) -> RefIdx {
        match self.mappings.get(sym) {
            Some(idx) => RefIdx::Resolved(*idx),
            None => RefIdx::Unresolved,
        }
    }
}

impl Pass<Symbol> for NameResolveCtx {
    fn next_origin(&mut self) -> OriginIdx {
        let old = self.current;
        self.current = self.current.next();

        old
    }

    fn pre_condition(_fir: &Fir<Symbol>) {}

    fn post_condition(_fir: &Fir) {}

    fn transform(&mut self, fir: Fir<Symbol>) -> Fir {
        self.insert_nodes(&fir);

        fir.nodes.into_iter().fold(Fir::default(), |fir, kv| {
            let (origin, node) = kv;

            let resolved = self.resolve_symbol(&node.data);

            let new_kind = match node.kind {
                Kind::Call { generics, args, .. } => Kind::Call {
                    to: resolved,
                    generics,
                    args,
                },
                // nothing to do for other types of nodes
                kind => kind,
            };

            fir.append(Node {
                data: (),
                origin,
                kind: new_kind,
            })
        })
    }
}

pub trait NameResolve {
    fn name_resolve(self) -> Fir;
}

impl NameResolve for Fir<Symbol> {
    #[must_use]
    fn name_resolve(self) -> Fir {
        let mut ctx = NameResolveCtx {
            current: OriginIdx::default(),
            mappings: HashMap::new(),
        };

        ctx.pass(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn name_resolve_simple() {
        let fir = Fir::<Symbol> {
            nodes: HashMap::new(),
        }
        // we declare a function "a"
        .append(Node {
            data: Symbol::from("a"),
            origin: OriginIdx(0),
            kind: Kind::FnDeclaration {
                generics: vec![],
                args: vec![],
            },
        })
        // we add a call to "a"
        .append(Node {
            data: Symbol::from("a"),
            origin: OriginIdx(1),
            kind: Kind::Call {
                to: RefIdx::Unresolved,
                generics: vec![],
                args: vec![],
            },
        });

        let fir = fir.name_resolve();

        let call = &fir.nodes[&OriginIdx(1)];

        if let Kind::Call { to, .. } = call.kind {
            assert_eq!(to, RefIdx::Resolved(OriginIdx(0)))
        } else {
            panic!()
        }
    }
}
