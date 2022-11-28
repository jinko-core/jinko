use std::collections::HashMap;

use fir::{Fir, Kind, Node, OriginIdx, Pass, RefIdx};
use symbol::Symbol;

struct NameResolveCtx {
    current: OriginIdx,
    _mappings: HashMap<Symbol, OriginIdx>,
}

impl Pass for NameResolveCtx {
    fn next_origin(&self) -> OriginIdx {
        // FIXME: this is bugged. Needs to update self.current
        self.current.next()
    }

    fn pre_condition(_fir: &Fir) {}

    fn post_condition(_fir: &Fir) {}

    fn transform(&mut self, fir: Fir) -> Fir {
        let (def, call) = create_invalid_relationship();

        fir.append(def).append(call)
    }
}

fn create_invalid_relationship() -> (Node, Node) {
    let origin = OriginIdx::default();
    let call = origin.next();

    (
        Node {
            origin,
            location: None,
            kind: Kind::Constant(RefIdx::Unresolved),
        },
        Node {
            origin: call,
            location: None,
            kind: Kind::Call {
                to: RefIdx::Resolved(origin),
                generics: vec![],
                args: vec![],
            },
        },
    )
}

pub trait NameResolve {
    fn name_resolve(self) -> Self;
}

impl NameResolve for Fir {
    fn name_resolve(self) -> Self {
        let mut ctx = NameResolveCtx {
            current: OriginIdx::default(),
            _mappings: HashMap::new(),
        };

        ctx.pass(self)
    }
}
