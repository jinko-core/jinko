//! ShuntingYard parses operators and operands according to operator precedence,
//! returning a BinaryOp in the end

use std::collections::LinkedList;

use crate::instruction::{BinaryOp, Instruction, Operator};

use super::box_construct::BoxConstruct;
use super::constructs::Construct;
use super::tokens::Token;

use nom::{branch::alt, error::ErrorKind, Err, IResult};

type Stack<T> = LinkedList<T>;

pub struct ShuntingYard {
    operands: Stack<Box<dyn Instruction>>,
    operators: Stack<Operator>,

    value: Option<BinaryOp>,
}

impl ShuntingYard {
    fn is_valid(&self) -> bool {
        match self.value {
            None => self.operators.len() >= 1 && self.operands.len() >= 2,
            Some(_) => self.operators.len() >= 1 && self.operands.len() >= 1,
        }
    }

    fn add_to_output(&mut self) {
        let first_v = self.operands.pop_front().unwrap();
        let op = self.operators.pop_front().unwrap();

        // "Take" the value to replace it, if it exists
        self.value = match self.value.take() {
            None => {
                // We don't always want to pop this one
                let second_v = self.operands.pop_front().unwrap();
                Some(BinaryOp::new(second_v, first_v, op))
            }
            Some(val) => Some(BinaryOp::new(Box::new(val), first_v, op)),
        }
    }

    fn operator<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        // FIXME: Don't unwrap?
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let (input, op) = alt((
            Token::add,
            Token::sub,
            Token::mul,
            Token::div,
            Token::left_parenthesis,
            Token::right_parenthesis,
        ))(input)?;

        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let op = Operator::new(op);

        // We can unwrap since we check that the stack is not empty
        while !self.operators.is_empty()
            && self.operators.front().unwrap().precedence() > op.precedence()
            && self.operators.front().unwrap() != &Operator::LeftParenthesis
        {
            if !self.is_valid() {
                return Err(Err::Error((
                    "Not a valid binary expression",
                    ErrorKind::Many1,
                )));
            }

            self.add_to_output();
        }

        if op == Operator::LeftParenthesis {
            self.operators.push_front(op);
        } else if op == Operator::RightParenthesis {
            while self.operators.front() != Some(&Operator::LeftParenthesis) {
                if !self.is_valid() {
                    return Err(Err::Error((
                        "Unfinished parenthesis - missing ')'",
                        ErrorKind::Many1,
                    )));
                }
                self.add_to_output();
            }

            // Discard the left parenthesis and check if it exists
            if self.operators.pop_front() != Some(Operator::LeftParenthesis) {
                return Err(Err::Error((
                    "Unstarted right parenthesis - missing '('",
                    ErrorKind::Many1,
                )));
            }
        } else {
            self.operators.push_front(op);
        }

        Ok((input, ()))
    }

    fn operand<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, expr) = alt((
            BoxConstruct::function_call,
            Construct::constant,
            BoxConstruct::variable,
        ))(input)?;

        self.operands.push_front(expr);

        Ok((input, ()))
    }

    fn handle_token<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let (input, _) = match input.chars().next() {
            // FIXME: Don't panic here
            None => return Err(Err::Error(("Not a valid binary expression", nom::error::ErrorKind::OneOf))),
            Some(c) => match Token::is_operator(c) {
                true => self.operator(input)?,
                false => self.operand(input)?,
            },
        };

        let (input, _) = Token::maybe_consume_whitespaces(input)?;

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
    pub fn parse(i: &str) -> IResult<&str, BinaryOp> {
        let mut sy = ShuntingYard::new();

        let mut input = i.clone();

        match sy.handle_token(input) {
            // FIXME: Don't say fuck
            Err(nom::Err::Error(_)) => {
                return Err(Err::Error(("Not a valid binary expression", nom::error::ErrorKind::Many1)))
            }
            Err(e) => return Err(e),
            Ok((new_i, _)) => {
                input = new_i;

                loop {
                    match sy.handle_token(input) {
                        Err(nom::Err::Error(_)) => break,
                        Err(e) => return Err(e),
                        Ok((new_i, _)) => {
                            if new_i == input {
                                break;
                            }

                            input = new_i;
                        }
                    }
                }
            }
        }

        // We are done, pop everything from the different stacks
        while !sy.operators.is_empty() {
            if !sy.is_valid() {
                return Err(Err::Error((
                    "Not a valid binary expression",
                    ErrorKind::Many1,
                )));
            }

            sy.add_to_output();
        }

        match sy.value.take() {
            Some(v) => Ok((input, v)),
            None => Err(Err::Error((
                "Not a valid binary expression",
                ErrorKind::Many1,
            ))),
        }
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

    #[test]
    fn t_sy_valid_parentheses_priority() {
        let output = ShuntingYard::parse("(3 + 1) * 2").unwrap().1;
        let l_ref = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(3)),
            Operator::Add,
        );
        let reference = BinaryOp::new(Box::new(l_ref), Box::new(JinkInt::from(2)), Operator::Mul);

        assert_eq!(output.operator(), reference.operator());
    }

    // FIXME: Add more tests with more operators
}
