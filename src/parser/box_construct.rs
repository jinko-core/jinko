//! This module simply wraps all constructs by returning a Boxed value instead. This is
//! useful for alternative parsing where multiple types of expressions are allowed, but
//! only one return type is valid

use crate::instruction::Instruction;

pub struct BoxConstruct;

impl BoxConstruct {
    /// Call a `Construct` and box the return value
    fn new<T: Instruction + 'static>(
        input: &'static str,
        construct: &dyn Fn(&str) -> nom::IResult<&str, T>,
    ) -> nom::IResult<&'static str, Box<dyn Instruction>> {
        let (input, value) = construct(input)?;

        Ok((input, Box::new(value)))
    }
}
