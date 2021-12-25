//! The Loop instruction is used for repeating instructions. They can be of three
//! different kinds, `for`, `while` or `loop`.

use crate::instruction::{Block, FunctionCall, InstrKind, Instruction, Var};
use crate::typechecker::{CheckedType, TypeCtx};
use crate::Generic;
use crate::{log, Context, FromObjectInstance, JkBool, ObjectInstance, TypeCheck};

/// What kind of loop the loop block represents: Either a for Loop, with a variable and
/// a range expression, a while loop with just an upper bound, or a loop with no bound
/// at all
#[derive(Clone)]
pub enum LoopKind {
    For(Var, Box<dyn Instruction>),
    While(Box<dyn Instruction>),
    Loop,
}

/// The Loop block struct. Contains the block to execute, as well as the kind of loop
/// it represents.
#[derive(Clone)]
pub struct Loop {
    kind: LoopKind,
    block: Block,
}

impl Loop {
    pub fn new(kind: LoopKind, block: Block) -> Loop {
        Loop { kind, block }
    }
}

impl Instruction for Loop {
    fn kind(&self) -> InstrKind {
        self.block.kind()
    }

    fn print(&self) -> String {
        match &self.kind {
            LoopKind::For(var, range) => format!(
                "for {} in {} {}\n",
                var.name(),
                range.print(),
                self.block.print()
            ),
            LoopKind::While(condition) => {
                format!("while {} {}\n", condition.print(), self.block.print())
            }
            LoopKind::Loop => format!("loop {}\n", self.block.print()),
        }
    }

    fn execute(&self, ctx: &mut Context) -> Option<ObjectInstance> {
        match &self.kind {
            LoopKind::Loop => loop {
                log!("loop enter");
                self.block.execute(ctx)?;
                log!("loop exit");
            },
            LoopKind::While(cond) => {
                let cond = cond.execute(ctx)?;
                log!("while enter");
                while JkBool::from_instance(&cond).rust_value() {
                    self.block.execute(ctx)?;
                }
                log!("while exit");
            }
            LoopKind::For(var, range_expression) => {
                log!("foreach enter");
                // Let's break down the implementation for the following loop
                // ```
                // for i in range(0, 10) { /* exec() */ }
                // ```

                // We enter a scope to declare our internal variables
                ctx.scope_enter();

                // We create new variables in the scope, with the required iterator's
                // name from the user. We can create these variables with special names
                // since we are only using them from the interpreter. Here, we prefix
                // them with a plus sign to make sure they do not interact with the user.
                // ```
                // +inner = range(0, 10);
                // +iterator = iter(+inner);
                //
                // +maybe_value = value(+iterator);
                // if !+maybe_value.is_some() { break }
                //
                // i = +maybe_iter_value.unpack(); // This is our <iter_value>
                //
                // // We can now do our first execution, and then repeat part of the
                // // above process
                // /* exec() */
                //
                // // FIXME: This needs to change. Ideally, we'd want to mutate
                // the `+iterator` variable
                // +iterator = next(+iterator);
                // +maybe_value = current(+iterator);
                // if !+maybe_value.is_some() { break }
                //
                // i = +maybe_iter_value.unpack();
                // /* exec() */
                // ```

                // Let's create our names
                let inner_name = String::from("+inner");
                let iterator_name = String::from("+iterator");
                let maybe_name = String::from("+maybe_value");

                // Now, let's create our variables
                let mut inner = Var::new(inner_name);
                let mut iterator = Var::new(iterator_name);
                let mut maybe = Var::new(maybe_name);
                let mut iter_value = Var::new(var.name().to_owned());

                // We execute the iterable expression
                // `+inner = range(0, 10)`
                inner.set_instance(range_expression.execute(ctx).unwrap());
                ctx.add_variable(inner.clone()).unwrap();

                // We construct the iterator from the iterable expression
                // `+iterator = iter(+inner)`
                let mut iter_constructor = FunctionCall::new(String::from("iter"), vec![], vec![]);
                iter_constructor.add_arg(Box::new(inner.clone()));
                iterator.set_instance(iter_constructor.execute(ctx).unwrap());

                // We fetch the first value from the iterator: `value(+iterator)`
                // This call will be reused multiple times!
                let mut iterator_value = FunctionCall::new(String::from("value"), vec![], vec![]);
                iterator_value.add_arg(Box::new(iterator.clone()));

                // We check if `+maybe_value` contains `Some` or `Nothing`: `is_some(+maybe_value)`
                // This call will be reused multiple times!
                let mut maybe_is_some = FunctionCall::new(String::from("is_some"), vec![], vec![]);
                maybe_is_some.add_arg(Box::new(maybe.clone()));

                // We advance the iterator: `next(+iterator)`
                // This call will be reused multiple times!
                let mut iterator_next = FunctionCall::new(String::from("next"), vec![], vec![]);
                iterator_next.add_arg(Box::new(iterator.clone()));

                let mut maybe_unpack = FunctionCall::new(String::from("unpack"), vec![], vec![]);
                maybe_unpack.add_arg(Box::new(maybe.clone()));

                // Now we can declare our variables in the context
                ctx.add_variable(iterator.clone()).unwrap();
                ctx.add_variable(maybe.clone()).unwrap();
                ctx.add_variable(iter_value.clone()).unwrap();

                maybe.set_instance(iterator_value.execute(ctx).unwrap());
                ctx.replace_variable(maybe.clone()).unwrap();

                let maybe_is_nothing = {
                    let instance = maybe_is_some.execute(ctx).unwrap();
                    !JkBool::from_instance(&instance).0
                };

                if maybe_is_nothing {
                    ctx.scope_exit();
                    return None;
                }

                iter_value.set_instance(maybe_unpack.execute(ctx).unwrap());
                ctx.replace_variable(iter_value.clone()).unwrap();

                loop {
                    self.block.execute(ctx);

                    iterator.set_instance(iterator_next.execute(ctx).unwrap());
                    ctx.replace_variable(iterator.clone()).unwrap();
                    maybe.set_instance(iterator_value.execute(ctx).unwrap());
                    ctx.replace_variable(maybe.clone()).unwrap();

                    let maybe_is_nothing = {
                        let instance = maybe_is_some.execute(ctx).unwrap();
                        !JkBool::from_instance(&instance).0
                    };

                    if maybe_is_nothing {
                        break;
                    }

                    iter_value.set_instance(maybe_unpack.execute(ctx).unwrap());
                    ctx.replace_variable(iter_value.clone()).unwrap();
                }

                ctx.scope_exit();
                log!("foreach exit");
            }
        }

        // FIXME: Add logic. Right now they only return on error, not the actual value
        None
    }
}

impl TypeCheck for Loop {
    fn resolve_type(&self, ctx: &mut TypeCtx) -> CheckedType {
        self.block.resolve_type(ctx)
    }
}

impl Generic for Loop {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::instruction::FunctionCall;
    use crate::jinko;
    use crate::{JkInt, ToObjectInstance};

    #[test]
    fn pretty_print_loop() {
        let b = Block::new();
        let l = Loop::new(LoopKind::Loop, b);

        assert_eq!(l.print().as_str(), "loop {\n}\n")
    }

    #[test]
    fn pretty_print_for() {
        let r = Box::new(FunctionCall::new("iter".to_owned(), vec![], vec![]));
        let b = Block::new();
        let l = Loop::new(LoopKind::For(Var::new("i".to_owned()), r), b);

        assert_eq!(l.print().as_str(), "for i in iter() {\n}\n")
    }

    #[test]
    fn pretty_print_while() {
        let r = Box::new(Block::new());
        let b = Block::new();
        let l = Loop::new(LoopKind::While(r), b);

        assert_eq!(l.print().as_str(), "while {\n} {\n}\n")
    }

    #[test]
    #[ignore]
    fn tc_valid_loop_blocks() {
        jinko! {
            // FIXME: Don't ignore once for loop behavior is implemented
            // l0 = for value in range { value }

            mut i = 0;
            while i < 15 { i = i + 1 }

            l2 = loop { i = i + 1; i }
        };
    }

    #[test]
    fn valid_for_block() {
        let ctx = jinko! {
            mut counter = 0;
            for i in range(0, 15) {
                counter = counter + 1;
            }
        };

        let counter = ctx.get_variable("counter").unwrap();
        assert_eq!(counter.instance(), JkInt::from(15).to_instance());
    }

    #[test]
    fn valid_for_block_without_execution() {
        jinko! {
            for i in range(0, 0) {
            }
        };
    }
}
