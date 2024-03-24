//! [`Fire`] stands for [`Fir`] Executor, or [`Fir`] Engine. Its role is to interpret code represented in an [`Fir`].

// TODO:
//     How do we deal with returns in this system?
//     How do we deal with them when they are nested in an `if` block or equivalent? Something like the `ControlFlow` enum as a return value? Either keep going or return until RefIdx?
//     Keep information in the context? (`self`)
// TODO: Transform all `RefIdx` to `OriginIdx` before this? Add a trait extension to make RefIdx: Deref<OriginIdx> specifically here?

pub mod instance;

use std::collections::HashMap;
use std::ops::ControlFlow;

use instance::Instance;

use fir::{Fir, Kind, Node, OriginIdx, RefIdx};
use flatten::FlattenData;

mod outside;

// Emit a fatal error from the [`Fire`].
// macro_rules! fire_fatal {
//     ($fmt:literal $($args:tt),*) => {
//         unreachable!(
//             concat!(
//                 concat!("{}:{}: ", $fmt),
//                 ": this is an interpreter error."),
//             $($args,)* core::file!(), core::line!()
//         ),
//     };
// }

// FIXME: UGLY: do we want to keep this?
/// This useful constant helps you avoid typing `ControlFlow::Continue(())` in all our functions
#[allow(non_upper_case_globals)]
const KeepGoing: ControlFlow<EarlyExit> = ControlFlow::Continue(());

/// Allows for an early return from a function, or to control the execution flow in a loop using `break` and `continue`
enum EarlyExit {
    Return(OriginIdx),
    // Break(OriginIdx),
    // Continue(OriginIdx),
}

pub trait Interpret {
    fn interpret(&self) -> Option<Instance>;
}

impl Interpret for Fir<FlattenData<'_>> {
    fn interpret(&self) -> Option<Instance> {
        let mut fire = Fire {
            gc: GarbaJKollector::new(),
            fir: self,
        };

        // Start the fire >:)
        fire.start()
    }
}

/// It's called [`GarbaJKollector`] because this way you have "jk" in the middle of the word :)
pub struct GarbaJKollector(HashMap<OriginIdx, Instance>);

// FIXME: Add documentation for all methods
impl GarbaJKollector {
    fn new() -> GarbaJKollector {
        GarbaJKollector(HashMap::new())
    }

    // TODO: The functions in this `impl` needs to take a T: IntoOrigin where we implement the trait for both OriginIdx and RefIdx

    fn allocate(&mut self, key: OriginIdx, value: Instance) {
        // if we allocate the same value twice, this is an interpreter error
        // FIXME: this is not specifically true - e.g. calling a function twice will allocate twice to the function's bindings, which is fine?
        self.0.insert(key, value);
    }

    fn copy(&mut self, to_copy: &RefIdx, key: OriginIdx) {
        if let Some(instance) = self.0.get(&to_copy.expect_resolved()) {
            self.allocate(key, instance.clone())
        }
    }

    fn transfer(&mut self, to_move: &RefIdx, key: OriginIdx) {
        self.copy(to_move, key)
    }

    fn lookup(&self, key: &OriginIdx) -> Option<&Instance> {
        self.0.get(key)
    }
}

struct Fire<'ast, 'fir> {
    // this will also take care of garbage collection so we must be careful
    gc: GarbaJKollector,
    // TODO: Store a &Fir here
    fir: &'fir Fir<FlattenData<'ast>>,
}

// Keep that on the back burner for now on... this works, but since the API might change a lot especially vis a vis return types for `fire_*` functions,
// we shouldn't use it yet
// struct Access<'ctx, 'ast, 'fir>(&'ctx mut Fire<'ast, 'fir>, &'fir Node<FlattenData<'ast>>);

// impl<'ctx, 'ast, 'fir> Access<'ctx, 'ast, 'fir> {
//     fn fire(&mut self) {
//         self.0.fire_node(self.1)
//     }
// }

impl<'ast, 'fir> Fire<'ast, 'fir> {
    // FIXME: Documentation
    // TODO: Rename? `node`? `node_from_ref`? `rnode`? `ref`? `view`?
    // should this return an `AccessedNode` or w/ever which we can then `.fire()`?
    fn access(&self, r: &RefIdx) -> &'fir Node<FlattenData<'ast>> {
        &self.fir.nodes[&r.expect_resolved()]
    }

    #[must_use]
    fn fire_block(
        &mut self,
        node: &Node<FlattenData<'_>>,
        stmts: &[RefIdx],
    ) -> ControlFlow<EarlyExit> {
        stmts
            .iter()
            .try_for_each(|node| self.fire_node(self.access(node)))?;

        if let Some(last_stmt) = stmts.last() {
            if let Kind::Return(_) = self.access(last_stmt).kind {
                self.gc.transfer(last_stmt, node.origin)
            }
        }

        KeepGoing
    }

    // FIXME: Does this need "_c"?
    #[must_use]
    fn fire_constant(
        &mut self,
        node: &Node<FlattenData<'_>>,
        _c: &RefIdx,
    ) -> ControlFlow<EarlyExit> {
        use ast::{Node, Value};

        let ast = node.data.ast.node();

        let instance = match &ast.node {
            Node::Constant(Value::Integer(i)) => Instance::from(*i),
            Node::Constant(Value::Float(f)) => Instance::from(*f),
            Node::Constant(Value::Bool(b)) => Instance::from(*b),
            Node::Constant(Value::Char(c)) => Instance::from(*c),
            Node::Constant(Value::Str(s)) => Instance::from(s),
            _ => unreachable!(),
        };

        self.gc.allocate(node.origin, instance);

        KeepGoing
    }

    #[must_use]
    fn fire_call(
        &mut self,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
        args: &[RefIdx],
    ) -> ControlFlow<EarlyExit> {
        let def = self.access(to);
        let (block, def_args) = match &def.kind {
            Kind::Function { block, args, .. } => (block, args),
            _ => unreachable!(),
        };

        args.iter().enumerate().try_for_each(|(i, arg)| {
            self.fire_node(self.access(arg))?;
            self.gc.transfer(arg, def_args[i].expect_resolved());

            KeepGoing
        });

        // FIXME: We need to add bindings here between the function's variables and the arguments given to the call
        match block {
            None => {
                let result = outside::perform_call(&self.gc, def, args);
                if let Some(instance) = result {
                    self.gc.allocate(node.origin, instance);
                }

                KeepGoing
            }
            Some(block) => {
                // here, we handle the firing of the block particularly as we want to catch early returns
                let result = match self.fire_reference(block) {
                    ControlFlow::Break(EarlyExit::Return(returned_value)) => {
                        RefIdx::Resolved(returned_value)
                    }
                    // FIXME: Can we use the wildcard here?
                    _ => *block,
                };

                self.gc.transfer(&result, node.origin);

                KeepGoing
            }
        }
    }

    #[must_use]
    fn fire_binding(
        &mut self,
        node: &Node<FlattenData<'_>>,
        to: &Option<RefIdx>,
    ) -> ControlFlow<EarlyExit> {
        if let Some(to) = to {
            self.fire_reference(to)?;

            self.gc.transfer(to, node.origin);
        }

        KeepGoing
    }

    #[must_use]
    fn fire_return(
        &mut self,
        node: &Node<FlattenData<'_>>,
        expr: &Option<RefIdx>,
    ) -> ControlFlow<EarlyExit> {
        if let Some(returned) = expr {
            self.fire_reference(returned)?;

            self.gc.transfer(returned, node.origin);
        } // FIXME: Allocate None otherwise?

        ControlFlow::Break(EarlyExit::Return(node.origin))
    }

    #[must_use]
    fn fire_reference(&mut self, node_ref: &RefIdx) -> ControlFlow<EarlyExit> {
        self.fire_node(self.access(node_ref))
    }

    #[must_use]
    fn fire_condition(
        &mut self,
        node: &Node<FlattenData<'_>>,
        condition: &RefIdx,
        true_block: &RefIdx,
        false_block: Option<&RefIdx>,
    ) -> ControlFlow<EarlyExit> {
        let t = Instance::from(true);
        let f = Instance::from(false);

        self.fire_reference(condition)?;

        let condition_result = self.gc.lookup(&condition.expect_resolved());

        let to_run = match condition_result {
            Some(boolean) if boolean == &t => Some(true_block),
            Some(boolean) if boolean == &f => false_block,
            _ => {
                unreachable!("{}:{}: the conditional did not evaluate to `true` or `false`. this is an interpreter error.", file!(), line!())
            }
        };

        // TODO: Is that correct?
        if let Some(block) = to_run {
            self.fire_reference(block)?;

            self.gc.transfer(block, node.origin);
        }

        KeepGoing
    }

    fn fire_node_ref(
        &mut self,
        node: &Node<FlattenData<'_>>,
        to: &RefIdx,
    ) -> ControlFlow<EarlyExit> {
        self.fire_reference(to)?;
        self.gc.transfer(to, node.origin);
        KeepGoing
    }

    #[must_use]
    fn fire_node(&mut self, node: &Node<FlattenData<'_>>) -> ControlFlow<EarlyExit> {
        match &node.kind {
            Kind::Constant(c) => self.fire_constant(node, c),
            Kind::Statements(stmts) => self.fire_block( node, stmts),
            Kind::Call {
                to,
                args,
                .. /* FIXME: Generics should be empty at this point */
            } => self.fire_call( node, to, args),
            Kind::Binding { to, .. } => self.fire_binding(node, to),
            Kind::NodeRef(to) => self.fire_node_ref(node, to),
            Kind::Return(expr) => self.fire_return(node, expr),
            // Kind::TypeReference(r) => self.traverse_type_reference( node, r),
            // Kind::Generic { default } => self.traverse_generic( node, default),
            // Kind::Type { generics, fields } => self.traverse_type( node, generics, fields),
            // Kind::Function {
            //     generics,
            //     args,
                // return_type,
            //     block,
            // } => self.traverse_function( node, generics, args, return_type, block),
            // Kind::Assignment { to, from } => self.traverse_assignment( node, to, from),
            // Kind::Instantiation {
            //     to,
            //     generics,
            //     fields,
            // } => self.traverse_instantiation( node, to, generics, fields),
            // Kind::TypeOffset { instance, field } => {
            //     self.traverse_type_offset( node, instance, field)
            // }
            Kind::Conditional {
                condition,
                true_block,
                false_block,
            } => self.fire_condition(node, condition, true_block, false_block.as_ref()),
            // Kind::Loop { condition, block } => self.traverse_loop( node, condition, block),
            _ => KeepGoing,
        }
    }

    fn start(&mut self) -> Option<Instance> {
        let entry_point = self.fir.nodes.last_key_value()?;
        let entry_point = &entry_point.1;
        let stmts = match &entry_point.kind {
            Kind::Statements(stmts) => stmts,
            _ => unreachable!(
                "{}:{}: expected list of statements for node {entry_point:?}. this is an interpreter error.",
                file!(),
                line!(),
            ),
        };

        let result = match self.fire_block(entry_point, stmts) {
            ControlFlow::Break(EarlyExit::Return(result)) => result,
            _ => entry_point.origin,
        };

        self.gc.lookup(&result).cloned()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use builtins::AppendAstBuiltins;
    use flatten::FlattenAst;
    use name_resolve::NameResolve;
    use typecheck::TypeCheck;

    macro_rules! ast {
        ($($toks:tt)*) => {
            {
                let ast = xparser::ast!(
                    type false;
                    type true;
                    type bool = false | true;
                    type unit;
                    type char;
                    type int;
                    type float;
                    type string;
                    $($toks)*
                );

                ast.append_builtins().unwrap()
            }
        }
    }

    macro_rules! fir {
        ($ast:expr) => {
            $ast.flatten().name_resolve().unwrap().type_check().unwrap()
        };
    }

    #[test]
    fn last_value() {
        let ast = ast! {
            "jinko"
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("jinko")))
    }

    #[test]
    fn call() {
        let ast = ast! {
            func id(x: int) -> int { x }

            id(15)
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from(15)))
    }

    #[test]
    fn nested_call() {
        let ast = ast! {
            func id(x: string) -> string { x }

            id(id(id(id("jinko"))))
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("jinko")))
    }

    #[test]
    fn conditional() {
        let ast = ast! {
            func halloween(b: bool) -> string {
                {
                    {
                        {
                            {
                                if b {
                                    return "boo"
                                } else {
                                    return "foo"
                                }
                            }
                        }
                    }
                }
            }

            halloween(true)
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("boo")))
    }

    #[test]
    fn conditional_else() {
        let ast = ast! {
            func halloween(b: bool) -> string {
                if b {
                    return "boo"
                } else {
                    return "foo"
                }
            }

            halloween(false)
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("foo")))
    }

    #[test]
    fn early_return() {
        let ast = ast! {
            func halloween(b: bool) -> string {
                if b {
                    return "boo"
                };


                "foo"
            }

            halloween(true)
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("boo")))
    }

    #[test]
    fn nested_return() {
        let ast = ast! {
            func halloween() -> string {
                { { { {
                    return "boo";
                } } } }
            }

            halloween()
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("boo")))
    }

    #[test]
    fn nested_return_inner_fn() {
        let ast = ast! {
            func halloween() -> string {
                func inner() -> string {
                    { { { {
                        return "boo";
                    } } } }
                };

                "different string"
            }

            halloween()
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("different string")))
    }

    #[test]
    fn return_from_fn_not_block() {
        let ast = ast! {
            func halloween() -> string {
                where x = {
                    {
                        {
                            {
                                {
                                    {
                                        return "one";
                                    }
                                }
                            }
                        }
                    }
                };


                return "two";
            }

            halloween()
        };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from("one")));
    }

    #[test]
    fn arithmetic() {
        let ast = ast! { 14 * 2 };

        let result = fir!(ast).interpret();
        assert_eq!(result, Some(Instance::from(28)));
    }
}
