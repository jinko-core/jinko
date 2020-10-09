//! ShuntingYard parses operators and operands according to operator precedence,
//! returning a BinaryOp in the end

use std::collections::LinkedList;

use crate::instruction::{BinaryOp, Instruction, Operator};

use super::constructs::Construct;
use super::box_construct::BoxConstruct;
use super::tokens::Token;

use nom::{branch::alt, IResult};

type Stack<T> = LinkedList<T>;

pub struct ShuntingYard {
    operands: Stack<Box<dyn Instruction>>,
    operators: Stack<Operator>,

    value: Option<BinaryOp>,
}

impl ShuntingYard {
    fn operator<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let (input, op) = alt((Token::add, Token::sub, Token::mul, Token::div))(input)?;

        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let op = Operator::new(op);

        self.operators.push_front(op);

        Ok((input, ()))
    }

    fn operand<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let (input, expr) = alt((
                BoxConstruct::function_call,
                Construct::constant,
                ))(input)?;

        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        self.operands.push_front(expr);

        Ok((input, ()))
    }

    /// Create a new, empty ShuntingYard parser
    fn new() -> ShuntingYard {
        ShuntingYard {
            operators: Stack::new(),
            operands: Stack::new(),
            value: None,
        }
    }

    /// Create a BinaryOp from an input string, executing the shunting yard
    /// algorithm
    pub fn parse(input: &str) -> IResult<&str, BinaryOp> {
        let mut parser = ShuntingYard::new();

        let (input, _) = parser.operand(input)?;
        let (input, _) = parser.operator(input)?;
        let (input, _) = parser.operand(input)?;

        // FIXME: Don't unwrap
        let rhs = parser.operands.pop_front().unwrap();
        let lhs = parser.operands.pop_front().unwrap();
        let op = parser.operators.pop_front().unwrap();

        Ok((input, BinaryOp::new(lhs, rhs, op)))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::*;

    #[test]
    fn t_sy_valid_add() {
        let output = ShuntingYard::parse("1 + 2").unwrap().1;
        let reference = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(2)),
            Operator::Add,
        );

        assert_eq!(output.operator(), reference.operator());
    }
}
