//! ShuntingYard parses operators and operands according to operator precedence,
//! returning a BinaryOp in the end

use std::collections::LinkedList;

use crate::instruction::{BinaryOp, Instruction, Operator};

use super::box_construct::BoxConstruct;
use super::constructs::Construct;
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

        // We can unwrap since we check that the stack is not empty
        while !self.operators.is_empty()
            && self.operators.front().unwrap().precedence() > op.precedence()
            && self.operators.front().unwrap() != &Operator::LeftParenthesis
        {
            // "Take" the value to replace it
            self.value = match self.value.take() {
                None => {
                    let rhs = self.operands.pop_front().unwrap();
                    let lhs = self.operands.pop_front().unwrap();
                    let op = self.operators.pop_front().unwrap();
                    Some(BinaryOp::new(lhs, rhs, op))
                }
                Some(val) => {
                    let rhs = self.operands.pop_front().unwrap();
                    let op = self.operators.pop_front().unwrap();
                    Some(BinaryOp::new(Box::new(val), rhs, op))
                }
            }
        }

        self.operators.push_front(op);

        Ok((input, ()))
    }

    fn operand<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let (input, expr) = alt((BoxConstruct::function_call, Construct::constant))(input)?;

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
        let mut sy = ShuntingYard::new();

        let (input, _) = sy.operand(input)?;
        let (input, _) = sy.operator(input)?;
        let (input, _) = sy.operand(input)?;

        let rhs = sy.operands.pop_front().unwrap();
        let lhs = sy.operands.pop_front().unwrap();
        let op = sy.operators.pop_front().unwrap();

        sy.value = Some(BinaryOp::new(lhs, rhs, op));

        // FIXME: No unwrap
        Ok((input, sy.value.take().unwrap()))
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

    #[test]
    fn t_sy_valid_mul() {
        let output = ShuntingYard::parse("1 * 2").unwrap().1;
        let reference = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(2)),
            Operator::Mul,
        );

        assert_eq!(output.operator(), reference.operator());
    }

    #[test]
    fn t_sy_valid_normal_priority() {
        let output = ShuntingYard::parse("1 * 2 + 3").unwrap().1;
        let l_ref = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(2)),
            Operator::Mul,
        );
        let reference = BinaryOp::new(Box::new(l_ref), Box::new(JinkInt::from(3)), Operator::Add);

        assert_eq!(output.operator(), reference.operator());
    }

    #[test]
    fn t_sy_valid_back_priority() {
        let output = ShuntingYard::parse("3 + 1 * 2").unwrap().1;
        let l_ref = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(2)),
            Operator::Mul,
        );
        let reference = BinaryOp::new(Box::new(l_ref), Box::new(JinkInt::from(3)), Operator::Add);

        assert_eq!(output.operator(), reference.operator());
    }
}
