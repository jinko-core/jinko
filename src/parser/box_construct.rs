//! This module simply wraps all `Constructs` by returning a Boxed value instead. This is
//! useful for alternative parsing where multiple types of expressions are allowed, but
//! only one return type is valid.
//!
//! For example, `Construct::function_call` returns a `FunctionCall`. If we want a
//! construct to return either a `FunctionCall` or a `Block` (for example), then we're
//! stuck, because we can't return either a `FunctionCall` or a `Block` in Rust. However
//! `Block` and `FunctionCall` are both `Instruction`s. We could make all `Constructs`
//! return a `Box<dyn Instruction>`, but this creates unnecessary allocations and
//! complexifies things. Instead, we can generate functions that return the correct
//! type when necessary. This way, `BoxConstruct::function_call` and `BoxConstruct::block`
//! wrap the return value of `Construct::function_call` and `Construct::block` in a box,
//! allowing to use them simultaneously when parsing multiple types of constructs.

use crate::{parser::Construct, Instruction};

macro_rules! box_construct {
    ($func:ident) => {
        pub fn $func(input: &str) -> nom::IResult<&str, Box<dyn Instruction>> {
            BoxConstruct::wrap(input, Box::new(Construct::$func))
        }
    };
}

pub struct BoxConstruct;

impl BoxConstruct {
    /// Call a `Construct` and box the return value
    fn wrap<T: 'static + Instruction>(
        input: &str,
        construct: Box<dyn FnOnce(&str) -> nom::IResult<&str, T>>,
    ) -> nom::IResult<&str, Box<dyn Instruction>> {
        let (input, value) = construct(input)?;

        Ok((input, Box::new(value)))
    }

    box_construct! {type_instantiation}
    box_construct! {function_call}
    box_construct! {function_declaration}
    box_construct! {ext_declaration}
    box_construct! {variable}
    box_construct! {block}
    box_construct! {jinko_inst}
    box_construct! {any_loop}
    box_construct! {var_assignment}
    box_construct! {if_else}
    box_construct! {type_declaration}
    box_construct! {test_declaration}
    box_construct! {mock_declaration}
    box_construct! {incl}
    box_construct! {method_call}
    box_construct! {field_access}
    box_construct! {extra}
}
