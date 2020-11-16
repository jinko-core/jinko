//! ShuntingYard parses operators and operands according to operator precedence,
//! returning a BinaryOp in the end

use crate::instruction::{BinaryOp, Instruction, Operator};
use crate::utils::{Queue, Stack};

use super::box_construct::BoxConstruct;
use super::constructs::Construct;
use super::tokens::Token;

use nom::{branch::alt, error::ErrorKind, Err, IResult};

pub struct ShuntingYard {
    operators: Stack<Operator>,
    output: Queue<Box<dyn Instruction>>,
}

impl ShuntingYard {

    fn reduce_output(&mut self) {
        // FIXME: No unwrap
        let rhs = self.output.pop().unwrap();
        let lhs = self.output.pop().unwrap();
        let op = self.operators.pop().unwrap();

        self.output.push(Box::new(BinaryOp::new(lhs, rhs, op)))
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
        if op != Operator::LeftParenthesis && op != Operator::RightParenthesis {
            while !self.operators.is_empty()
            // FIXME: Cleanup
            && (self.operators.peek().unwrap().precedence() > op.precedence()
            || (self.operators.peek().unwrap().precedence() == op.precedence() && op.is_left_associative()))
            {
                self.reduce_output()
            }
            self.operators.push(op)
        } else if op == Operator::LeftParenthesis {
            self.operators.push(op);
        } else if op == Operator::RightParenthesis {
            // FIXME: Don't unwrap
            while self.operators.peek().unwrap() != &Operator::LeftParenthesis {
                self.reduce_output()
            }

            if self.operators.peek().unwrap() == &Operator::LeftParenthesis {
                // Discard the parenthesis
                self.operators.pop();
            }
        }

        Ok((input, ()))
    }

    fn operand<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, expr) = alt((
            BoxConstruct::function_call,
            Construct::constant,
            BoxConstruct::variable,
        ))(input)?;

        self.output.push(expr);

        Ok((input, ()))
    }

    fn handle_token<'i>(&mut self, input: &'i str) -> IResult<&'i str, ()> {
        let (input, _) = Token::maybe_consume_whitespaces(input)?;

        let (input, _) = match input.chars().next() {
            None => {
                return Err(Err::Error((
                    "Not a valid binary expression",
                    nom::error::ErrorKind::OneOf,
                )))
            }
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
            output: Queue::new(),
        }
    }

    /// Create a BinaryOp from an input string, executing the shunting yard
    /// algorithm
    pub fn parse(i: &str) -> IResult<&str, Box<dyn Instruction>> {
        let mut sy = ShuntingYard::new();

        let mut input = i.clone();

        match sy.handle_token(input) {
            // FIXME: Don't say fuck
            Err(nom::Err::Error(_)) => {
                return Err(Err::Error((
                    "Not a valid binary expression",
                    nom::error::ErrorKind::Many1,
                )))
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
            sy.reduce_output();
        }

        // FIXME: Don't unwrap, check length and stuff
        Ok((input, sy.output.pop().unwrap()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::*;

    #[test]
    fn t_sy_valid_add() {
        let boxed_output = ShuntingYard::parse("1 + 2").unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();
        let reference = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(2)),
            Operator::Add,
        );

        assert_eq!(output.operator(), reference.operator());
    }

    #[test]
    fn t_sy_valid_mul() {
        let boxed_output = ShuntingYard::parse("1 * 2").unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();
        let reference = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(2)),
            Operator::Mul,
        );

        assert_eq!(output.operator(), reference.operator());
    }

    #[test]
    fn t_sy_valid_normal_priority() {
        let boxed_output = ShuntingYard::parse("1 * 2 + 3").unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();
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
        let boxed_output = ShuntingYard::parse("3 + 1 * 2").unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();
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
        let boxed_output = ShuntingYard::parse("(3 + 1) * 2").unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();
        let l_ref = BinaryOp::new(
            Box::new(JinkInt::from(1)),
            Box::new(JinkInt::from(3)),
            Operator::Add,
        );
        let reference = BinaryOp::new(Box::new(l_ref), Box::new(JinkInt::from(2)), Operator::Mul);

        assert_eq!(output.operator(), reference.operator());
    }

    #[test]
    fn t_sy_valid_parentheses_priority_reverse() {
        let boxed_output = ShuntingYard::parse("2 * (3 + 1)").unwrap().1;
        let output = boxed_output.downcast_ref::<BinaryOp>().unwrap();
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
