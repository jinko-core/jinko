//! This module contains checks performed on the [`Fir`] at runtime. Should these checks
//! fail, the crate will panic.

use std::collections::HashSet;
use std::fmt::Debug;

use crate::{Fir, Kind, RefIdx};

// FIXME: Use something like miette for the error messages?

impl<T: Debug> Fir<T> {
    /// Check that all origins in the [`Fir`] are unique
    ///
    /// # Panic
    ///
    /// This function panic if the provided [`Fir`] contains an origin that is present multiple times.
    fn check_unique_origins(&self) {
        let mut origins = HashSet::new();
        self.nodes.iter().for_each(|kv| {
            let origin = kv.0;

            match origins.get(origin) {
                // FIXME: Do a nice error here
                // FIXME: Add more info like both nodes having the same origin
                // c\n\n{:#?}\n\nrefers to\n\n{:#?}\n\n[reference: {:?}]", $node, self.nodes[&origin], $ref),
                Some(_) => panic!("non-unique `OriginIdx` detected in `Fir`"),
                None => origins.insert(origin),
            };
        })
    }

    /// Check if the [`Fir`] only contains links between entities allowed to link together.
    /// For example, this asserts that there are no calls that have been resolved as calls to constant literals.
    ///
    /// # Panic
    ///
    /// This function panic if the provided [`Fir`] contains invalid relationships.
    fn check_valid_links(&self) {
        macro_rules! check {
            ($ref:expr => Some ( $kind:pat ), $node:expr) => {
                if let Some(_e) = $ref {
                    check!(_e => $kind, $node)
                }
            };
            ($ref:expr => $kind:pat, $node:expr) => {
                if let RefIdx::Resolved(origin) = $ref {
                    match self.nodes[&origin].kind {
                        $kind => {}
                        // FIXME: Do a nice error here
                        _ => panic!("invalid relationship detected in `Fir`:\n\n{:#?}\n\nrefers to\n\n{:#?}\n\n[reference: {:?}]", $node, self.nodes[&origin], $ref),
                    }
                }
            };
            (@$iter:expr => $kind:pat, $node:expr) => {
                $iter.iter().for_each(|value| check!(value => $kind, $node))
            };
        }

        self.nodes.iter().for_each(|kv| {
            let node = &kv.1;
            match &node.kind {
                // FIXME: This is missing a bunch of valid "checks". For example, checking that a call's argument can
                // point to an if-else expression. Basically, to anything that's an expression actually.
                // Should we split the fir::Kind into fir::Kind::Stmt and fir::Kind::Expr? Or does that not make sense?
                Kind::Constant(r) => check!(r => Kind::TypeReference(_), node),
                Kind::TypedValue { value, ty } => {
                    check!(ty => Kind::Type { .. }, node);
                    // `value` can link to basically anything
                    check!(value => Kind::Call { .. }
                        | Kind::Constant(_)
                        | Kind::Instantiation { .. }
                        | Kind::TypedValue { .. }
                        | Kind::Binding { .. }, node);
                }
                Kind::TypeReference(to) => check!(to => Kind::TypeReference(_) | Kind::Generic { .. }, node),
                Kind::Generic {
                    default: Some(default),
                } => check!(default => Kind::Type { .. }, node),
                Kind::Function {
                    generics,
                    args,
                    return_type,
                    block,
                } => {
                    check!(@generics => Kind::Generic { .. }, node);
                    check!(@args => Kind::TypedValue { .. }, node);
                    check!(return_type => Some(Kind::Type { .. }), node);
                    check!(block => Some(Kind::Statements(_)), node);
                }
                Kind::Call {
                    to,
                    generics,
                    args,
                } => {
                    check!(to => Kind::Function { .. }, node);
                    check!(@generics => Kind::Type { .. }, node);
                    check!(@args => Kind::TypedValue { .. } | Kind::Constant(_), node);
                }
                Kind::Type {
                    generics,
                    fields,
                } => {
                    check!(@generics => Kind::Generic { .. }, node);
                    check!(@fields => Kind::TypedValue { .. }, node);
                }
                Kind::Binding { to: _ } => {
                    // FIXME: Check `to` as well
                }
                Kind::Assignment { to, from: _ } => {
                    check!(to => Kind::TypedValue { .. }, node);
                    // FIXME: Check `from` as well
                }
                Kind::Instantiation {
                    to,
                    generics,
                    fields,
                } => {
                    check!(to => Kind::Type { .. }, node);
                    check!(@generics => Kind::Type { .. }, node);
                    check!(@fields => Kind::TypedValue { .. }, node);
                }
                Kind::TypeOffset { instance: _, .. } => {
                    // can point to anything? FIXME
                }
                Kind::Conditional { .. /* FIXME: Missing condition, true_block and false_block */ } => {
                    // check!(condition => Kind::Constant(_) | Kind::TypedValue { .. } | Kind::Call { .. }, node);
                },
                Kind::Loop { condition: _, block } => {
                    // FIXME: Check condition?
                    check!(block => Kind::Statements(_), node);
                }
                // Statements can point to anything
                Kind::Statements(_) => {}
                // Nothing to do for generics without a default
                Kind::Generic { default: None } => {}
                // Return statements can point to anything // FIXME: Is that true?
                Kind::Return(_) => {},

            }
        })
    }

    /// Check that the [`Fir`] is in a valid state. This means that all [`OriginIdx`] should be unique,
    /// and that all links between nodes should be allowed
    ///
    /// # Panic
    ///
    /// This function panics if the [`Fir`] is in a state deemed invalid
    pub fn check(&self) {
        self.check_unique_origins();
        self.check_valid_links();
    }
}

#[cfg(test)]
mod tests {
    use crate::{Node, OriginIdx};

    use super::*;

    // #[test]
    // #[should_panic]
    // fn cyclic_fir() {}

    #[test]
    #[should_panic]
    fn non_unique_origin() {
        let fir = Fir::default()
            .append(Node {
                data: (),
                origin: OriginIdx(0),
                kind: Kind::Generic { default: None },
            })
            .append(Node {
                data: (),
                origin: OriginIdx(0),
                kind: Kind::Generic { default: None },
            });

        fir.check();
    }

    #[test]
    #[should_panic]
    fn invalid_link() {
        let fir = Fir::default()
            // generic
            .append(Node {
                data: (),
                origin: OriginIdx(0),
                kind: Kind::Generic { default: None },
            })
            // call to a generic
            .append(Node {
                data: (),
                origin: OriginIdx(1),
                kind: Kind::Call {
                    to: RefIdx::Resolved(OriginIdx(0)),
                    generics: vec![],
                    args: vec![],
                },
            });

        fir.check();
    }
}
