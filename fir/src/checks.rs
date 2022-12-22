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
                Kind::Constant(r) => check!(r => Kind::Type { .. }, node),
                Kind::TypedValue { value, ty } => {
                    check!(ty => Kind::Type { .. }, node);
                    // `value` can link to basically anything
                    check!(value => Kind::Call { .. } | Kind::Constant(_) | Kind::Instantiation { .. } | Kind::TypedValue { .. }, node);
                }
                Kind::Generic {
                    default: Some(default),
                } => check!(default => Kind::Type { .. }, node),
                Kind::FnDeclaration {
                    generics,
                    args,
                    return_type,
                } => {
                    check!(@generics => Kind::Generic { .. }, node);
                    check!(@args => Kind::TypedValue { .. }, node);
                    if let Some(ret_ty) = return_type {
                        check!(ret_ty => Kind::Type { .. }, node);
                    }
                }
                Kind::Call {
                    to,
                    generics,
                    args,
                } => {
                    check!(to => Kind::FnDeclaration { .. }, node);
                    check!(@generics => Kind::Type { .. }, node);
                    check!(@args => Kind::TypedValue { .. }, node);
                }
                Kind::Type {
                    generics,
                    fields,
                } => {
                    check!(@generics => Kind::Generic { .. }, node);
                    check!(@fields => Kind::TypedValue { .. }, node);
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
                // Statements can point to anything
                Kind::Statements(_) => {}
                // Nothing to do for generics without a default
                Kind::Generic { default: None } => {}
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