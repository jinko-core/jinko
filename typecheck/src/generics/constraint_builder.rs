//! The goal of the [`ConstraintBuilder`] is to build a list of required
//! constraints when calling or instantiating generic functions and types. It is
//! perfectly valid for a generic call to have zero constraints - in fact, generic
//! type instantiations will not have any constraints, as they do not perform any
//! function calls in their declarations. As a consequence, only function calls
//! will be considered for all examples in this module.

use std::collections::HashMap;

use fir::{iter::TreeLike, Fallible, Fir, Kind, Node, OriginIdx, RefIdx, Traversal};
use flatten::FlattenData;

// this should probably be a HashMap instead - multiple generics per generic call, multiple constraints per generic
type Constraints = HashMap<RefIdx /* A generic */, Vec<()>>;
type GenericCall = OriginIdx;

// TODO: Alright, so how do we want to organize that constraint map. A list of constraints per invocation/call?
type ConstraintMap = HashMap<GenericCall, Constraints>;

// TODO: Do we need the type context here?
#[derive(Default)]
pub struct ConstraintBuilder {
    constraints: ConstraintMap,
}

// No errors?
#[derive(Debug)]
pub struct Error;

struct FnCtx<'fir, 'ast> {
    generics: &'fir [RefIdx],
    args: &'fir [RefIdx],
    return_type: Option<RefIdx>,
    stmts: &'fir [RefIdx],
    fir: &'fir Fir<FlattenData<'ast>>,
}

struct Woobler {
    pub(crate) to_see: RefIdx,
    pub(crate) seen: bool,
}

impl Woobler {
    pub fn new(to_see: RefIdx) -> Woobler {
        Woobler {
            to_see,
            seen: false,
        }
    }
}

impl<'ast, 'fir> TreeLike<'ast, 'fir, FlattenData<'ast>> for Woobler {
    fn visit_reference(&mut self, fir: &Fir<FlattenData<'ast>>, reference: &RefIdx) {
        if *reference == self.to_see {
            self.seen = true;
        }

        self.visit(fir, &reference.expect_resolved())
    }
}

impl<'fir, 'ast> FnCtx<'fir, 'ast> {
    pub fn from_invocation(
        fir: &'fir Fir<FlattenData<'ast>>,
        resolved_call: &RefIdx,
    ) -> FnCtx<'fir, 'ast> {
        let definition = &fir[resolved_call];

        match &definition.kind {
            Kind::Function {
                generics,
                args,
                return_type,
                block: Some(block),
            } => {
                let block = &fir[block];

                let stmts = match &block.kind {
                    Kind::Statements(stmts) => stmts,
                    _ => unreachable!(),
                };

                FnCtx {
                    generics: generics.as_slice(),
                    args: args.as_slice(),
                    return_type: *return_type,
                    stmts: stmts.as_slice(),
                    fir,
                }
            }
            _ => unreachable!(),
        }
    }

    // FIXME: Should we consume self here instead?
    fn collect_constraints(&self) -> Constraints {
        let FnCtx {
            generics,
            args,
            return_type,
            stmts,
            fir,
        } = self;

        // we first have to build a list of possibly constrained args - if an arg's type is in the list of generics?
        // do we just do that based on name resolution? is that information stored in the typectx? do we even handle
        // it at the moment?
        let constrained_args: Vec<&RefIdx> = args
            .iter()
            .filter(|arg| {
                // first, we get the actual typed value we are dealing with - args are flattened as bindings, but
                // we're looking for the underlying value who's being bound.
                let binding = match fir[*arg].kind {
                    Kind::Binding { to } => to,
                    _ => unreachable!(),
                };

                let arg_ty = match fir[&binding].kind {
                    Kind::TypedValue { ty, .. } => ty,
                    _ => unreachable!(),
                };

                let arg_ty = match fir[&arg_ty].kind {
                    Kind::TypeReference(to) => to,
                    _ => unreachable!(),
                };

                generics.contains(&arg_ty)
            })
            .collect();

        let constraints = stmts
            .iter()
            .filter_map(|stmt| {
                let mut woobler = Woobler::new(*constrained_args[0]);

                woobler.visit_reference(fir, stmt);

                woobler.seen.then_some(stmt)
            })
            .fold(Constraints::new(), |mut constraints, _constraint| {
                // here we mostly want to insert or update
                constraints.insert(generics[0], vec![]);

                constraints
            });

        constraints
    }
}

impl Traversal<FlattenData<'_>, Error> for ConstraintBuilder {
    fn traverse_call(
        &mut self,
        fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
        generics: &[RefIdx],
        _args: &[RefIdx],
    ) -> Fallible<Error> {
        let fn_ctx = FnCtx::from_invocation(fir, to);
        let constraints = fn_ctx.collect_constraints();

        // get the definition
        // run through it
        // build constraints

        self.constraints.insert(node.origin, constraints);

        Ok(())
    }

    fn traverse_instantiation(
        &mut self,
        _fir: &Fir<FlattenData<'_>>,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
        generics: &[RefIdx],
        _fields: &[RefIdx],
    ) -> Fallible<Error> {
        // not much to do here? but we should still build a constraint so that this gets turned into a mono' request, which will be easier for the Monormorphizer

        self.constraints.insert(node.origin, HashMap::new());

        Ok(())
    }
}
