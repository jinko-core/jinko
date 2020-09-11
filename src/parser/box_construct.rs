//! This module simply wraps all constructs by returning a Boxed value instead. This is
//! useful for alternative parsing where multiple types of expressions are allowed, but
//! only one return type is valid

use crate::instruction::Instruction;

use super::constructs::Construct;

macro_rules! box_construct {
    ($func:ident) => {
        pub fn $func(input: &str) -> nom::IResult<&str, Box<dyn Instruction>> {
            BoxConstruct::new(input, Box::new(Construct::$func))
        }
    };
}

pub struct BoxConstruct;

impl BoxConstruct {
    /// Call a `Construct` and box the return value
    fn new<T: 'static + Instruction>(
        input: &str,
        construct: Box<dyn FnOnce(&str) -> nom::IResult<&str, T>>,
    ) -> nom::IResult<&str, Box<dyn Instruction>> {
        let (input, value) = construct(input)?;

        Ok((input, Box::new(value)))
    }

    box_construct!{function_call}
}
